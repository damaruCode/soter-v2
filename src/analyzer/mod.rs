use crate::abstraction::Abstraction;
use crate::ast::TypedCore;
use crate::state_space::*;
use crate::util::graphviz::GraphBuilder;
use crate::util::AstHelper;
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
        }
    }

    pub fn run(
        &mut self,
        graph_builder: &mut GraphBuilder<K, V>,
    ) -> (SetMap<Pid, ProcState<K, V>>, Store<K, V>) {
        // This terminates because it assumes a fixpoint implementation
        while let Some(item) = self.queue.pop_front() {
            let (new_items, revisit_items) = item.process(
                &self.ast_helper,
                &mut self.mailboxes,
                &mut self.store,
                &self.abstraction,
                &mut self.module_env,
                &self.seen,
                graph_builder,
            );

            for item in new_items {
                // NOTE cloning here might become a memory issue
                if let Some(items) = self.seen.get_mut(&item.pid) {
                    if items.contains(&item) {
                        continue;
                    }
                }
                self.seen.push(item.pid.clone(), item.clone());
                self.queue.push_back(item);
            }

            for item in revisit_items {
                self.queue.push_back(item);
            }
        }

        return (self.seen.clone(), self.store.clone());
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
        graph_builder: &mut GraphBuilder<K, V>,
    ) -> (Vec<Self>, Vec<Self>);
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
        graph_builder: &mut GraphBuilder<K, V>,
    ) -> (Vec<Self>, Vec<Self>) {
        match self.prog_loc_or_pid {
            ProgLocOrPid::ProgLoc(pl) => {
                log::debug!("{:#?}\nAst:{}", self, ast_helper.get(pl))
            }
            ProgLocOrPid::Pid(_) => log::debug!("{:#?}", self),
        }

        let (new, revisit) = match &self.prog_loc_or_pid {
            ProgLocOrPid::Pid(pid) => {
                abs_pop_let_pid(pid, self, store, seen, abstraction, ast_helper)
            }
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
                    abs_receive(r, self, mailboxes, store, module_env, ast_helper)
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
                                    //TODO ABS_POP_LET_VALUEADDR
                                    match &self.prog_loc_or_pid {
                                        // ABS_POP_LET_PID
                                        ProgLocOrPid::Pid(_pid) => todo!(),
                                        ProgLocOrPid::ProgLoc(pl) => match ast_helper.get(*pl) {
                                            // ABS_POP_LET_VALUELIST
                                            TypedCore::AstList(_al) => todo!(),
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
        };

        for node in new {
            graph_builder.add_node(&node);

            graph_builder.add_edge(&self, &node);
        }

        for node in revisit {
            graph_builder.add_edge(&self, &node);
        }

        (new, revisit)
    }
}
