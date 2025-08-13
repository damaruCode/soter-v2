use crate::ast::Index;
use crate::ast::TypedCore;
use crate::state_space::r#abstract::*;
use crate::util::AstHelper;
use crate::util::SetMap;

use std::collections::VecDeque;

mod allocation_schemes;
pub use allocation_schemes::*;

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
            store: Store::init(),
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

    fn get_data_dependencies(&self, pid: &Pid) -> Vec<ProcState<K, V>> {
        let mut dependencies = Vec::new();
        match self.seen.get(pid) {
            Some(set) => {
                for state in set {
                    match state.prog_loc_or_pid {
                        ProgLocOrPid::ProgLoc(location) => {
                            match &self.ast_helper.get(location) {
                                TypedCore::Receive(_) => {
                                    // NOTE cloning here might become a memory issue
                                    dependencies.push(state.clone());
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
        dependencies
    }

    fn get_value_dependencies(&self, vaddr: &V) -> Vec<ProcState<K, V>> {
        let mut dependencies = Vec::new();
        for (_, states) in &self.seen.inner {
            for state in states {
                match state.prog_loc_or_pid {
                    ProgLocOrPid::ProgLoc(location) => match &self.ast_helper.get(location) {
                        TypedCore::Var(pl_var) => {
                            match state.env.inner.get(&pl_var) {
                                Some(pl_vaddr) => {
                                    if pl_vaddr == vaddr {
                                        // NOTE cloning here might become a memory issue
                                        dependencies.push(state.clone());
                                    }
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
        }
        dependencies
    }

    fn get_kontinuation_dependencies(&self, kaddr: &K) -> Vec<ProcState<K, V>> {
        let mut dependencies = Vec::new();
        for (_, states) in &self.seen.inner {
            for state in states {
                if state.k_addr != *kaddr {
                    continue;
                }
                match state.prog_loc_or_pid {
                    ProgLocOrPid::ProgLoc(location) => match &self.ast_helper.get(location) {
                        // TODO add the arms that are non-reducable
                        _ => {}
                    },
                    ProgLocOrPid::Pid(_) => {
                        // NOTE cloning here might become a memory issue
                        dependencies.push(state.clone());
                    }
                }
            }
        }
        dependencies
    }
}

pub trait WorkItem<K: KontinuationAddress, V: ValueAddress>: Eq + Clone {
    fn process(
        &self,
        ast_helper: &AstHelper,
        mailboxes: &mut Mailboxes<V>,
        store: &mut Store<K, V>,
        address_builder: &Box<dyn AddressBuilder<K, V>>,
    ) -> (Vec<Self>, Vec<Self>);
}

impl<K: KontinuationAddress, V: ValueAddress> WorkItem<K, V> for ProcState<K, V> {
    fn process(
        &self,
        ast_helper: &AstHelper,
        mailboxes: &mut Mailboxes<V>,
        store: &mut Store<K, V>,
        address_builder: &Box<dyn AddressBuilder<K, V>>,
    ) -> (Vec<Self>, Vec<Self>) {
        match self.prog_loc_or_pid {
            ProgLocOrPid::ProgLoc(pl) => {
                log::debug!("{:#?}\nAST - {:#?}", self, ast_helper.get(pl))
            }
            ProgLocOrPid::Pid(_) => log::debug!("{:#?}", self),
        }

        let mut v_new = Vec::new();
        let mut v_revisit = Vec::new();

        match &self.prog_loc_or_pid {
            ProgLocOrPid::Pid(_pid) => {
                // ABS_POP_LET_PID
            }
            ProgLocOrPid::ProgLoc(pl) => match ast_helper.get(*pl) {
                TypedCore::Module(m) => match &*m.defs.inner[0].scnd {
                    TypedCore::Fun(f) => match &*f.body {
                        // NOTE could have more clauses than one
                        TypedCore::Case(c) => match &c.clauses.inner[0] {
                            TypedCore::Clause(c) => {
                                let mut new_item = self.clone();
                                let index = (*c.body).get_index().unwrap();
                                new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(index);
                                v_new.push(new_item);
                            }
                            _ => panic!(),
                        },
                        _ => panic!(),
                    },
                    _ => panic!(),
                },
                TypedCore::Var(v) => match self.env.inner.get(v) {
                    Some(v) => match store.value.get(&v) {
                        Some(values) => {
                            for value in values {
                                // consider each
                                // non-deterministically
                                match value {
                                    Value::Closure(c) => {
                                        let mut new_item = self.clone();
                                        new_item.prog_loc_or_pid =
                                            ProgLocOrPid::ProgLoc(c.prog_loc);
                                        v_new.push(new_item);
                                    }
                                    Value::Pid(_) => {
                                        panic!("Unexpected value: Expected Closure not Pid")
                                    }
                                }
                            }
                        }
                        None => panic!("VAddr does not exist within value store"),
                    },
                    None => panic!("No VAddr exists for given Var"),
                },
                TypedCore::Apply(_apply) => {
                    // ABS_APPLY
                    panic!();
                }
                TypedCore::Call(_call) => {
                    // ABS_CALL
                    panic!();
                }
                TypedCore::LetRec(_let_rec) => {
                    // ABS_LETREC
                    panic!();
                }
                TypedCore::Case(_case) => {
                    // ABS_CASE
                    panic!();
                }
                TypedCore::Receive(_receive) => {
                    // ABS_RECEIVE
                    panic!();
                }
                TypedCore::PrimOp(_prim_op) => {
                    // NOTE This would require another
                    // match on the name. However, the PrimOps: self, spawn and send are the only
                    // ones being considered, so parsing them in the ast module would probably be
                    // more sensible

                    // ABS_SELF
                    // ABS_SPAWN
                    // ABS_SEND
                    panic!();
                }
                // TODO ABS_PUSH_DO
                TypedCore::Let(l) => {
                    // ABS_PUSH_LET
                    let mut new_item = self.clone();
                    new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc((*l.arg).get_index().unwrap());
                    new_item.k_addr = address_builder.new_kaddr(
                        &self,
                        &new_item.prog_loc_or_pid,
                        &new_item.env,
                        &new_item.time,
                    );

                    let kont = Kont::Let(
                        (&l.vars).into(),
                        (*l.body).get_index().unwrap(),
                        self.env.clone(),
                        self.k_addr.clone(),
                    );

                    store.kont.push(new_item.k_addr.clone(), kont);
                    v_new.push(new_item);
                }
                // ProgLoc is irreducible via the previous transition rules; it's a Value
                // We need to look at the continuation for the next computation
                _ => match store.kont.get(&self.k_addr) {
                    Some(konts) => {
                        // consider each possible continuation
                        for kont in konts {
                            match kont {
                                Kont::Let(_val_list, _body, _env, _k_addr) => {
                                    // ABS_POP_LET_CLOSURE
                                    // ABS_POP_LET_PID
                                    // ABS_POP_LET_VALUEADDR
                                    // ABS_POP_LET_VALUELIST
                                    panic!();
                                }
                                Kont::Do(_body, _env, _k_addr) => {
                                    // ABS_POP_DO
                                    panic!();
                                }
                                Kont::Stop => {
                                    // NOTE halt (successful)
                                }
                            }
                        }
                    }
                    None => {
                        // NOTE fail
                        panic!();
                    }
                },
            },
        };
        (v_new, v_revisit)
    }
}
