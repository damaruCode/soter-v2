use std::hash::Hash;

use crate::ast::TypedCore;
use crate::state_space::r#abstract::*;
use crate::util::WorkItem;

mod allocation_schemes;
pub use allocation_schemes::*;

pub enum TransitionError {
    ErroneousTransition,
    NoValidTransition,
}

pub struct Analyzer<'a, K: KontinuationAddress, V: ValueAddress> {
    current_program_state: State<'a, K, V>,
    address_builder: Box<dyn AddressBuilder<'a, K, V>>,
}

impl<'a, K: KontinuationAddress, V: ValueAddress> Analyzer<'a, K, V> {
    pub fn new(ast: &'a TypedCore, address_builder: Box<dyn AddressBuilder<'a, K, V>>) -> Self {
        let prog_loc = ProgLoc::init(ast);
        let k_addr = address_builder.init_kaddr(
            Pid::init(prog_loc.clone()),
            prog_loc,
            Env::init(),
            Time::init(),
        );

        Analyzer {
            address_builder,
            current_program_state: State::init(ast, k_addr),
        }
    }

    // pub fn step(&mut self) -> Result<(), TransitionError> {
    //     for (pid, proc_state) in &self.current_program_state.procs {
    //         match &proc_state.prog_loc_or_pid {
    //             ProgLocOrPid::Pid(_pid) => {
    //                 // ABS_POP_LET_PID
    //             }
    //             ProgLocOrPid::ProgLoc(prog_loc) => match prog_loc.get() {
    //                 TypedCore::Var(var) => match proc_state.env.get(&var.name) {
    //                     Some(vaddr) => match self.current_program_state.store.get_value(&vaddr) {
    //                         Some(values) => {
    //                             for value in values {
    //                                 // consider each
    //                                 // non-deterministically
    //                                 match value {
    //                                     Value::Closure(clo) => {
    //                                         self.current_program_state.procs.push(
    //                                             pid.clone(),
    //                                             ProcState::<K, V>::new(
    //                                                 pid.clone(),
    //                                                 ProgLocOrPid::ProgLoc(clo.prog_loc),
    //                                                 clo.env.clone(),
    //                                                 proc_state.k_addr.clone(),
    //                                                 proc_state.time.clone(),
    //                                             ),
    //                                         );
    //                                     }
    //                                     Value::Pid(_pid) => {
    //                                         panic!("Unexpected value: Expected Closure not Pid")
    //                                     }
    //                                 }
    //                             }
    //                         }
    //                         None => panic!("VAddr does not exist within value store"),
    //                     },
    //                     None => panic!("No VAddr exists for given Var"),
    //                 },
    //                 TypedCore::Apply(_apply) => {
    //                     // ABS_APPLY
    //                 }
    //                 TypedCore::Call(_call) => {
    //                     // ABS_CALL
    //                 }
    //                 TypedCore::LetRec(_let_rec) => {
    //                     // ABS_LETREC
    //                 }
    //                 TypedCore::Case(_case) => {
    //                     // ABS_CASE
    //                 }
    //                 TypedCore::Receive(_receive) => {
    //                     // ABS_RECEIVE
    //                 }
    //                 TypedCore::PrimOp(_prim_op) => {
    //                     // NOTE This would require another
    //                     // match on the name. However, the PrimOps: self, spawn and send are the only
    //                     // ones being considered, so parsing them in the ast module would probably be
    //                     // more sensible
    //
    //                     // ABS_SELF
    //                     // ABS_SPAWN
    //                     // ABS_SEND
    //                 }
    //                 // ABS_PUSH_DO
    //                 TypedCore::Let(_let) => {
    //                     // ABS_PUSH_LET
    //                 }
    //                 // ProgLoc is irreducible via the previous transition rules; it's a Value
    //                 // We need to look at the continuation for the next computation
    //                 _ => match self
    //                     .current_program_state
    //                     .store
    //                     .get_kont(&proc_state.k_addr)
    //                 {
    //                     Some(konts) => {
    //                         // consider each possible continuation
    //                         for kont in konts {
    //                             match kont {
    //                                 Kont::Let(_val_list, _body, _env, _k_addr) => {
    //                                     // ABS_POP_LET_CLOSURE
    //                                     // ABS_POP_LET_PID
    //                                     // ABS_POP_LET_VALUEADDR
    //                                     // ABS_POP_LET_VALUELIST
    //                                 }
    //                                 Kont::Do(_body, _env, _k_addr) => {
    //                                     // ABS_POP_DO
    //                                 }
    //                                 Kont::Stop => {
    //                                     // NOTE halt (successful)
    //                                 }
    //                             }
    //                         }
    //                     }
    //                     None => {
    //                         // NOTE fail
    //                     }
    //                 },
    //             },
    //         };
    //     }
    //
    //     Ok(())
    // }

    fn get_data_dependencies(&self, pid: &Pid) -> Vec<ProcState<K, V>> {
        let mut dependencies = Vec::new();
        match self.current_program_state.procs.get(pid) {
            Some(set) => {
                for state in set {
                    match &state.prog_loc_or_pid {
                        ProgLocOrPid::ProgLoc(location) => match location.get() {
                            TypedCore::Receive(_) => {
                                // NOTE cloning here might become a memory issue
                                dependencies.push(state.clone());
                            }
                            _ => {}
                        },
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
        for (_, state) in &self.current_program_state.procs {
            match &state.prog_loc_or_pid {
                ProgLocOrPid::ProgLoc(location) => match location.get() {
                    TypedCore::Var(pl_var) => {
                        match state.env.get(&pl_var.name) {
                            Some(pl_vaddr) => {
                                if pl_vaddr == *vaddr {
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
        dependencies
    }

    fn get_kontinuation_dependencies(&self, kaddr: &K) -> Vec<ProcState<K, V>> {
        let mut dependencies = Vec::new();
        for (_, state) in &self.current_program_state.procs {
            if state.k_addr != *kaddr {
                continue;
            }
            match &state.prog_loc_or_pid {
                ProgLocOrPid::ProgLoc(location) => match location.get() {
                    // TODO add the arms that are non-reducable
                    _ => {}
                },
                ProgLocOrPid::Pid(_) => {
                    // NOTE cloning here might become a memory issue
                    dependencies.push(state.clone());
                }
            }
        }
        dependencies
    }
}

#[derive(Clone)]
pub struct AnalyzerWorkItem<'a, K, V>
where
    K: KontinuationAddress,
    V: ValueAddress,
{
    proc_state: ProcState<'a, K, V>,
    store: Store<'a, K, V>,
    mailboxes: Mailboxes<'a, V>,
}
impl<'a, K, V> PartialEq for AnalyzerWorkItem<'a, K, V>
where
    K: KontinuationAddress,
    V: ValueAddress,
{
    fn eq(&self, other: &Self) -> bool {
        self.proc_state.eq(&other.proc_state)
    }
}
impl<'a, K, V> Eq for AnalyzerWorkItem<'a, K, V>
where
    K: KontinuationAddress,
    V: ValueAddress,
{
}
impl<'a, K, V> Hash for AnalyzerWorkItem<'a, K, V>
where
    K: KontinuationAddress,
    V: ValueAddress,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.proc_state.hash(state);
    }
}

impl<'a, K, V> WorkItem for AnalyzerWorkItem<'a, K, V>
where
    K: KontinuationAddress,
    V: ValueAddress + 'a,
{
    fn process(&self) -> (Vec<Self>, Vec<Self>) {
        let mut v_new = Vec::new();
        let mut v_revisit = Vec::new();

        match &self.proc_state.prog_loc_or_pid {
            ProgLocOrPid::Pid(_pid) => {
                // ABS_POP_LET_PID
            }
            ProgLocOrPid::ProgLoc(prog_loc) => match prog_loc.get() {
                TypedCore::Var(var) => match self.proc_state.env.get(&var.name) {
                    Some(vaddr) => match self.store.get_value(&vaddr) {
                        Some(values) => {
                            for value in values {
                                // consider each
                                // non-deterministically
                                match value {
                                    Value::Closure(clo) => {
                                        v_new.push(Self {
                                            proc_state: ProcState::<K, V>::new(
                                                self.proc_state.pid.clone(),
                                                ProgLocOrPid::ProgLoc(clo.prog_loc.clone()),
                                                clo.env.clone(),
                                                self.proc_state.k_addr.clone(),
                                                self.proc_state.time.clone(),
                                            ),
                                            store: self.store.clone(),
                                            mailboxes: self.mailboxes.clone(),
                                        });
                                    }
                                    Value::Pid(_pid) => {
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
                }
                TypedCore::Call(_call) => {
                    // ABS_CALL
                }
                TypedCore::LetRec(_let_rec) => {
                    // ABS_LETREC
                }
                TypedCore::Case(_case) => {
                    // ABS_CASE
                }
                TypedCore::Receive(_receive) => {
                    // ABS_RECEIVE
                }
                TypedCore::PrimOp(_prim_op) => {
                    // NOTE This would require another
                    // match on the name. However, the PrimOps: self, spawn and send are the only
                    // ones being considered, so parsing them in the ast module would probably be
                    // more sensible

                    // ABS_SELF
                    // ABS_SPAWN
                    // ABS_SEND
                }
                // ABS_PUSH_DO
                TypedCore::Let(_let) => {
                    // ABS_PUSH_LET
                }
                // ProgLoc is irreducible via the previous transition rules; it's a Value
                // We need to look at the continuation for the next computation
                _ => match self.store.get_kont(&self.proc_state.k_addr) {
                    Some(konts) => {
                        // consider each possible continuation
                        for kont in konts {
                            match kont {
                                Kont::Let(_val_list, _body, _env, _k_addr) => {
                                    // ABS_POP_LET_CLOSURE
                                    // ABS_POP_LET_PID
                                    // ABS_POP_LET_VALUEADDR
                                    // ABS_POP_LET_VALUELIST
                                }
                                Kont::Do(_body, _env, _k_addr) => {
                                    // ABS_POP_DO
                                }
                                Kont::Stop => {
                                    // NOTE halt (successful)
                                }
                            }
                        }
                    }
                    None => {
                        // NOTE fail
                    }
                },
            },
        };

        (v_new, v_revisit)
    }
}
