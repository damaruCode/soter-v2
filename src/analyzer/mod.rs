use crate::ast::TypedCore;
use crate::state_space::r#abstract::*;

mod allocation_schemes;
use allocation_schemes::*;

pub enum TransitionError {
    ErroneousTransition,
    NoValidTransition,
}

pub struct Analyzer<'a, K: KontinuationAddress, V: ValueAddress> {
    current_program_state: State<'a, K, V>,
}

impl<'a, K: KontinuationAddress, V: ValueAddress> Analyzer<'a, K, V> {
    pub fn new(ast: &'a TypedCore) -> Self {
        Analyzer {
            current_program_state: State::init(ast),
        }
    }

    pub fn step(&self) -> Result<State<K, V>, TransitionError> {
        for (_pid, proc_state) in &self.current_program_state.procs {
            let _state = match &proc_state.prog_loc_or_pid {
                ProgLocOrPid::Pid(_pid) => {
                    // ABS_POP_LET_PID
                }
                ProgLocOrPid::ProgLoc(prog_loc) => match prog_loc.get() {
                    TypedCore::Var(_var) => {
                        // ABS_NAME
                    }
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
                    // ProgLoc is irreducible via the previous transition rules; we call it a value
                    // We need to look at the continuation for the next computation
                    _ => match self
                        .current_program_state
                        .store
                        .get_kont(&proc_state.k_addr)
                    {
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
        }

        Ok(self.current_program_state.clone())
    }

    fn get_data_dependencies(&self, pid: &Pid) -> Vec<ProcState> {
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

    fn get_value_dependencies(&self, vaddr: &VAddr) -> Vec<ProcState> {
        let mut dependencies = Vec::new();
        for (_, state) in &self.current_program_state.procs {
            match &state.prog_loc_or_pid {
                ProgLocOrPid::ProgLoc(location) => match location.get() {
                    TypedCore::Var(pl_var) => {
                        // NOTE took out the indirection of var
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

    fn get_kontinuation_dependencies(&self, kaddr: &K) -> Option<Vec<ProcState<K, V>>> {
        // TODO
        None
    }
}
