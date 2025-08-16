use crate::ast::TypedCore;
use crate::state_space::r#abstract::*;
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
    address_builder: Box<dyn AddressBuilder<K, V>>,
    mailboxes: Mailboxes<V>,
    store: Store<K, V>,
    queue: VecDeque<ProcState<K, V>>,
    seen: SetMap<Pid, ProcState<K, V>>,
}

impl<'analyzer, K: KontinuationAddress, V: ValueAddress> Analyzer<'analyzer, K, V> {
    pub fn new(
        ast_helper: AstHelper<'analyzer>,
        address_builder: Box<dyn AddressBuilder<K, V>>,
    ) -> Self {
        let k_addr = address_builder.init_kaddr();
        Analyzer {
            ast_helper,
            address_builder,
            mailboxes: Mailboxes::init(),
            store: Store::init(k_addr.clone()),
            queue: VecDeque::from(vec![ProcState::init(k_addr)]),
            seen: SetMap::new(),
        }
    }

    pub fn run(&mut self) {
        // This terminates because it assumes a fixpoint implementation
        while let Some(item) = self.queue.pop_front() {
            let (new_items, revisit_items) = item.process(
                &self.ast_helper,
                &mut self.mailboxes,
                &mut self.store,
                &self.address_builder,
                &self.seen,
            );
            for item in revisit_items {
                self.queue.push_back(item);
            }

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
        }
    }
}

pub trait WorkItem<K: KontinuationAddress, V: ValueAddress>: Eq + Clone {
    fn process(
        &self,
        ast_helper: &AstHelper,
        mailboxes: &mut Mailboxes<V>,
        store: &mut Store<K, V>,
        address_builder: &Box<dyn AddressBuilder<K, V>>,
        seen: &SetMap<Pid, ProcState<K, V>>,
    ) -> (Vec<Self>, Vec<Self>);
}

impl<K: KontinuationAddress, V: ValueAddress> WorkItem<K, V> for ProcState<K, V> {
    fn process(
        &self,
        ast_helper: &AstHelper,
        mailboxes: &mut Mailboxes<V>,
        store: &mut Store<K, V>,
        address_builder: &Box<dyn AddressBuilder<K, V>>,
        seen: &SetMap<Pid, ProcState<K, V>>,
    ) -> (Vec<Self>, Vec<Self>) {
        //TODO Delete log
        match self.prog_loc_or_pid {
            ProgLocOrPid::ProgLoc(pl) => {
                log::debug!("{:#?}\nAST - {:#?}", self, ast_helper.get(pl))
            }
            ProgLocOrPid::Pid(_) => log::debug!("{:#?}", self),
        }

        match &self.prog_loc_or_pid {
            ProgLocOrPid::Pid(pid) => {
                abs_pop_let_pid(pid, self, store, seen, address_builder, ast_helper)
            }
            ProgLocOrPid::ProgLoc(pl) => match ast_helper.get(*pl) {
                TypedCore::Module(m) => {
                    abs_push_module(m, self, store, seen, address_builder, ast_helper)
                }
                TypedCore::Var(v) => abs_name(v, self, store),
                TypedCore::Apply(a) => abs_apply(a, self, store, ast_helper),
                TypedCore::Call(c) => {
                    abs_call(c, self, mailboxes, store, seen, ast_helper, address_builder)
                }
                TypedCore::LetRec(_let_rec) => todo!("ABS_LETREC"),
                TypedCore::Case(c) => abs_case(c, self, store, ast_helper),
                TypedCore::Receive(r) => abs_receive(r, self, mailboxes, store, ast_helper),
                TypedCore::PrimOp(_prim_op) => {
                    // NOTE This would require another
                    // match on the name. However, the PrimOps: self, spawn and send are the only
                    // ones being considered, so parsing them in the ast module would probably be
                    // more sensible
                    todo!("ABS_PRIMOP, ABS_SELF, ABS_SPAWN, ABS_SEND")
                }
                TypedCore::Let(l) => {
                    abs_push_let(l, self, store, seen, address_builder, ast_helper)
                }
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
                                                    var_list,
                                                    body,
                                                    &env,
                                                    &k_addr,
                                                    store,
                                                    seen,
                                                    address_builder,
                                                    ast_helper,
                                                );
                                            }
                                        },
                                    }
                                }
                                Kont::Module(index, env, k_addr) => {
                                    return abs_pop_module_fun(
                                        index,
                                        &env,
                                        self,
                                        store,
                                        seen,
                                        address_builder,
                                        ast_helper,
                                    )
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
                        // NOTE (fail)
                        todo!("Program seems to fail")
                    }
                },
            },
        }
    }
}
