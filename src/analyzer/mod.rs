use crate::abstraction::Abstraction;
use crate::ast::TypedCore;
use crate::state_space::*;
use crate::util::AstHelper;
use crate::util::Graph;
use crate::util::Node;
use crate::util::SetMap;

use std::collections::VecDeque;

mod dependency_checker;
mod transitions;

use transitions::*;

pub enum TransitionError {
    ErroneousTransition,
    NoValidTransition,
}

pub struct Analyzer<'analyzer, K: KontinuationAddress, V: ValueAddress> {
    ast_helper: AstHelper<'analyzer>,
    abstraction: Box<dyn Abstraction<K, V>>,
    module_env: Env<V>,
    mailboxes: Mailboxes<V>,
    store: Store<K, V>,
    queue: VecDeque<ProcState<K, V>>,
    seen: SetMap<Pid, ProcState<K, V>>,
    transition_graph: Graph<ProcState<K, V>, String>,
}

impl<'analyzer, K: KontinuationAddress, V: ValueAddress> Analyzer<'analyzer, K, V> {
    pub fn new(ast_helper: AstHelper<'analyzer>, abstraction: Box<dyn Abstraction<K, V>>) -> Self {
        let stop_k_addr = abstraction.stop_kaddr();
        Analyzer {
            ast_helper,
            abstraction,
            module_env: Env::init(),
            mailboxes: Mailboxes::init(),
            store: Store::init(stop_k_addr.clone()),
            queue: VecDeque::from(vec![ProcState::init(stop_k_addr)]),
            seen: SetMap::new(),
            transition_graph: Graph::new(),
        }
    }

    pub fn run(&mut self) -> (SetMap<Pid, ProcState<K, V>>, Mailboxes<V>, Store<K, V>) {
        // This terminates because it assumes a fixpoint implementation
        for node in self.queue.clone() {
            self.transition_graph.add_node(node);
        }

        while let Some(item) = self.queue.pop_front() {
            let (new_items, _revisit_items) = item.process(
                &self.ast_helper,
                &mut self.mailboxes,
                &mut self.store,
                &self.abstraction,
                &mut self.module_env,
                &self.seen,
            );

            for (new_proc_state, transition_name) in new_items {
                // NOTE cloning here might become a memory issue
                if let Some(seen_items) = self.seen.get_mut(&new_proc_state.pid) {
                    if seen_items.contains(&new_proc_state) {
                        self.transition_graph
                            .add_edge(
                                Node::new(item.clone()),
                                Node::new(new_proc_state.clone()),
                                transition_name,
                            )
                            .unwrap();
                        continue;
                    }
                }
                self.transition_graph.add_node(new_proc_state.clone());
                self.transition_graph
                    .add_edge(
                        Node::new(item.clone()),
                        Node::new(new_proc_state.clone()),
                        transition_name,
                    )
                    .unwrap();
                self.seen
                    .push(new_proc_state.pid.clone(), new_proc_state.clone());
                self.queue.push_back(new_proc_state);
            }

            // for (revisit_state, transition_name) in revisit_items {
            //     if self.queue.contains(&revisit_state) {
            //         continue;
            //     }
            //
            //     self.transition_graph
            //         .add_edge(
            //             Node::new(item.clone()),
            //             Node::new(revisit_state.clone()),
            //             format!("{} - revisit", transition_name),
            //         )
            //         .unwrap();
            //     self.queue.push_back(revisit_state);
            // }
        }

        return (
            self.seen.clone(),
            self.mailboxes.clone(),
            self.store.clone(),
        );
    }

    pub fn get_transition_graph(&self) -> Graph<ProcState<K, V>, String> {
        self.transition_graph.clone()
    }
}

pub trait WorkItem<K: KontinuationAddress, V: ValueAddress>: Eq + Clone {
    fn process(
        &self,
        ast_helper: &AstHelper,
        mailboxes: &mut Mailboxes<V>,
        store: &mut Store<K, V>,
        abstraction: &Box<dyn Abstraction<K, V>>,
        module_env: &mut Env<V>,
        seen: &SetMap<Pid, ProcState<K, V>>,
    ) -> (Vec<(Self, String)>, Vec<(Self, String)>);
}

impl<K: KontinuationAddress, V: ValueAddress> WorkItem<K, V> for ProcState<K, V> {
    fn process(
        &self,
        ast_helper: &AstHelper,
        mailboxes: &mut Mailboxes<V>,
        store: &mut Store<K, V>,
        abstraction: &Box<dyn Abstraction<K, V>>,
        module_env: &mut Env<V>,
        seen: &SetMap<Pid, ProcState<K, V>>,
    ) -> (Vec<(Self, String)>, Vec<(Self, String)>) {
        match self.prog_loc_or_pid {
            ProgLocOrPid::ProgLoc(pl) => {
                log::debug!("{:#?}\nAst:{}", self, ast_helper.get(pl))
            }
            ProgLocOrPid::Pid(_) => log::debug!("{:#?}", self),
        }

        match &self.prog_loc_or_pid {
            ProgLocOrPid::Pid(pid) => abs_pid(pid, self, store, seen, abstraction, ast_helper),
            ProgLocOrPid::ProgLoc(pl) => match ast_helper.get(*pl) {
                TypedCore::Module(m) => abs_module(m, self, store, module_env, abstraction),
                TypedCore::Var(v) => abs_name(v, self, store),
                TypedCore::Apply(a) => {
                    abs_apply(a, *pl, self, seen, store, abstraction, ast_helper)
                }
                TypedCore::Call(c) => {
                    abs_call(c, self, mailboxes, store, seen, ast_helper, abstraction)
                }
                TypedCore::LetRec(_let_rec) => todo!("ABS_LETREC"),
                TypedCore::Case(c) => abs_case(c, self, store, module_env, ast_helper),
                TypedCore::Receive(r) => {
                    abs_receive(r, self, mailboxes, store, seen, abstraction, ast_helper)
                }
                TypedCore::PrimOp(_prim_op) => todo!("ABS_PRIMOP, ABS_SELF, ABS_SPAWN, ABS_SEND"),
                TypedCore::Let(l) => abs_push_let(l, self, store, seen, abstraction, ast_helper),
                TypedCore::Seq(s) => abs_push_seq(s, self, store, seen, abstraction, ast_helper),
                // ProgLoc is irreducible via the previous transition rules; it's a Value
                // We need to look at the continuation for the next computation
                _ => match store.kont.get(&self.k_addr) {
                    Some(konts) => {
                        let konts = konts.clone();
                        // consider each possible continuation
                        for kont in konts {
                            match kont {
                                Kont::Let(var_list, body, env, k_addr) => {
                                    // NOTE ABS_POP_LET_VALUEADDR will probably be left out ---
                                    // where needed we consider each possible resolution of VAddrs
                                    match &self.prog_loc_or_pid {
                                        ProgLocOrPid::ProgLoc(pl) => match ast_helper.get(*pl) {
                                            TypedCore::AstList(_al) => {
                                                todo!("ABS_POP_LET_VALUELIST")
                                            }
                                            _ => {
                                                return abs_pop_let_closure(
                                                    self,
                                                    *pl,
                                                    &var_list,
                                                    body,
                                                    &env,
                                                    &k_addr,
                                                    store,
                                                    seen,
                                                    abstraction,
                                                    ast_helper,
                                                );
                                            }
                                        },
                                        _ => panic!(), //
                                    }
                                }
                                Kont::Seq(body, env, k_addr) => {
                                    return abs_pop_seq(self, body, &env, &k_addr)
                                }
                                Kont::Stop => {
                                    // NOTE (successful)
                                    return (Vec::new(), Vec::new());
                                }
                            }
                        }
                        panic!();
                    }
                    None => {
                        panic!()
                    }
                },
            },
        }
    }
}
