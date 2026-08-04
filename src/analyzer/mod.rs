use crate::abstraction::Abstraction;
use crate::ast::TypedCore;
use crate::state_space::*;
use crate::util::AstHelper;
use crate::util::Graph;
use crate::util::SetMap;

use std::collections::VecDeque;

mod dependency_checker;
mod match_helper;
mod transitions;

pub use match_helper::*;
use transitions::*;

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

    // Start fixpoint computation with WorkList-Algorithm
    pub fn run(&mut self) -> (SetMap<Pid, ProcState<K, V>>, Mailboxes<V>, Store<K, V>) {
        // This terminates because it assumes a fixpoint implementation
        for node in self.queue.clone() {
            self.transition_graph.add_node(node);
        }

        while let Some(item) = self.queue.pop_front() {
            // Computes new ProcStates and asses which have to be revisited
            let result = item.process(
                &self.ast_helper,
                &mut self.mailboxes,
                &mut self.store,
                &self.abstraction,
                &mut self.module_env,
                &self.seen,
            );

            for (new_proc_state, transition_name) in result.new {
                // NOTE cloning here might become a memory issue
                self.transition_graph.add_edge(
                    item.clone(),
                    new_proc_state.clone(),
                    transition_name,
                );

                // Skip if already seen
                if let Some(seen_items) = self.seen.get_mut(&new_proc_state.pid) {
                    if seen_items.contains(&new_proc_state) {
                        continue;
                    }
                }

                // Update seen and queue otherwise
                self.seen
                    .push(new_proc_state.pid.clone(), new_proc_state.clone());
                self.queue.push_back(new_proc_state);
            }

            for (revisit_state, transition_name) in result.revisit {
                // Skip if already queued
                if self.queue.contains(&revisit_state) {
                    continue;
                }

                self.transition_graph.add_edge(
                    item.clone(),
                    revisit_state.clone(),
                    format!("{} - revisit", transition_name),
                );

                // Update queue otherwise
                self.queue.push_back(revisit_state);
            }
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
    ) -> TransitionResult<K, V>;
}

impl<K: KontinuationAddress, V: ValueAddress> WorkItem<K, V> for ProcState<K, V> {
    // Decides which transition might be applicable
    fn process(
        &self,
        ast_helper: &AstHelper,
        mailboxes: &mut Mailboxes<V>,
        store: &mut Store<K, V>,
        abstraction: &Box<dyn Abstraction<K, V>>,
        module_env: &mut Env<V>,
        seen: &SetMap<Pid, ProcState<K, V>>,
    ) -> TransitionResult<K, V> {
        //logging
        match self.prog_loc_or_pid {
            ProgLocOrPid::ProgLoc(pl) => {
                log::debug!("{:#?}\nAst:{}", self, ast_helper.get(pl))
            }
            ProgLocOrPid::Pid(_) => log::debug!("{:#?}", self),
        }

        if self.failure_type != FailureType::None {
            return TransitionResult::new();
        }

        match &self.prog_loc_or_pid {
            ProgLocOrPid::Pid(pid) => abs_pid(pid, self, store, seen, abstraction, ast_helper),
            ProgLocOrPid::ProgLoc(pl) => match ast_helper.get(*pl) {
                TypedCore::Module(m) => {
                    abs_module(m, self, store, module_env, abstraction, ast_helper)
                }
                TypedCore::Var(v) => abs_name(v, self, store),
                TypedCore::Apply(a) => abs_apply(
                    a,
                    *pl,
                    self,
                    module_env,
                    seen,
                    store,
                    abstraction,
                    ast_helper,
                ),
                TypedCore::Call(c) => abs_call(
                    c,
                    self,
                    mailboxes,
                    store,
                    module_env,
                    seen,
                    ast_helper,
                    abstraction,
                ),
                TypedCore::LetRec(_let_rec) => todo!("ABS_LETREC"),
                TypedCore::Case(c) => abs_case(c, self, store, seen, abstraction, ast_helper),
                TypedCore::Receive(r) => {
                    abs_receive(r, self, mailboxes, store, seen, abstraction, ast_helper)
                }
                TypedCore::PrimOp(prim_op) => abs_primop(prim_op, self),
                TypedCore::Let(l) => abs_push_let(l, self, store, seen, abstraction, ast_helper),
                TypedCore::Seq(s) => abs_push_seq(s, self, store, seen, abstraction, ast_helper),
                // ProgLoc is irreducible via the previous transition rules; it's a Value
                // We need to look at the continuation for the next computation
                _ => match store.kont.get(&self.k_addr) {
                    Some(konts) => {
                        let mut result = TransitionResult::new();

                        let konts = konts.clone();
                        // consider each possible continuation
                        for kont in konts {
                            let mut res;
                            match kont {
                                Kont::Let(var_list, body, env, k_addr) => {
                                    // NOTE ABS_POP_LET_VALUEADDR will probably be left out ---
                                    // where needed we consider each possible resolution of VAddrs
                                    match &self.prog_loc_or_pid {
                                        ProgLocOrPid::ProgLoc(pl) => match ast_helper.get(*pl) {
                                            TypedCore::Tuple(_) | TypedCore::Cons(_) => {
                                                res = abs_pop_let_value_list(
                                                    self,
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
                                            _ => {
                                                res = abs_pop_let_closure(
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
                                    res = abs_pop_seq(self, body, &env, &k_addr);
                                }
                                Kont::Stop => {
                                    // NOTE (successful)
                                    res = TransitionResult::new();
                                }
                            }
                            result.append(&mut res);
                        }

                        result
                    }
                    None => {
                        panic!()
                    }
                },
            },
        }
    }
}
