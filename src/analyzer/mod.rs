use crate::ast::TypedCore;
use crate::state_space::r#abstract::{KAddr, Kont, ProcState, ProgLoc, ProgLocOrPid, State};
use crate::transition_system::{TransitionError, TransitionSystem, TypedTransition};

pub struct Analyzer<'a> {
    curr_state: State<'a>,
    proc_transition_system: TransitionSystem<
        ProcState<'a>,
        fn(&ProcState<'a>) -> Result<ProcState<'a>, TransitionError>,
    >,
}

impl<'a> Analyzer<'a> {
    pub fn new(&self, ast: &'a TypedCore) -> Self {
        let mut proc_transition_system: TransitionSystem<
            ProcState<'a>,
            fn(&ProcState<'a>) -> Result<ProcState<'a>, TransitionError>,
        > = TransitionSystem::init();

        proc_transition_system.register_transition(TypedTransition::new(/* NOTE dafuq */));

        Analyzer {
            curr_state: State::init(ast),
            proc_transition_system,
        }
    }

    pub fn step(&self) -> Result<State, TransitionError> {
        let mut new_state = self.curr_state.clone();

        for (pid, proc_state) in &self.curr_state.procs.inner {
            let set = new_state.procs.inner.get_mut(pid).unwrap();
            set.clear();

            match self.proc_transition_system.try_apply(proc_state) {
                Ok(new_proc_state) => set.insert(new_proc_state),
                Err(e) => return Err(e),
            };
        }

        Ok(new_state)
    }

    // ABS_NAME
    // ABS_APPLY
    // ABS_CALL
    // ABS_LETREC
    // ABS_CASE
    // ABS_RECEIVE
    // ABS_SELF
    // ABS_SPAWN
    // ABS_SEND
    // ABS_PUSH_DO
    // ABS_POP_DO
    // ABS_PUSH_LET
    fn t_push_let(proc_state: &'a ProcState) -> Result<ProcState<'a>, TransitionError> {
        if let ProgLocOrPid::ProgLoc(ref prog_loc) = proc_state.prog_loc_or_pid {
            if let TypedCore::Let(l) = prog_loc.get() {
                // TODO handle the .expect properly (in a standard way)
                let var_list = l.vars;
                let arg = &l.arg;
                let body = &l.body;

                // Push-Let
                let k_let = Kont::Let(
                    var_list,
                    ProgLoc::new(body),
                    proc_state.env.clone(),
                    proc_state.k_addr.clone(),
                );

                let k_addr = KAddr::new(
                    proc_state.pid.clone(),
                    prog_loc.clone(),
                    proc_state.env.clone(),
                    proc_state.time.clone(),
                );

                store.kont.push(k_addr.clone(), k_let); // TODO think about how to reference the
                                                        // new state

                return Ok(ProcState::new(
                    proc_state.pid.clone(),
                    ProgLocOrPid::ProgLoc(ProgLoc::new(arg)),
                    proc_state.env.clone(),
                    k_addr,
                    proc_state.time.clone(),
                ));
            }
        }

        Err(TransitionError::ErroneousTransition)
    }
    // ABS_POP_LET_CLOSURE
    // ABS_POP_LET_PID
    // ABS_POP_LET_VALUEADDR
    // ABS_POP_LET_VALUELIST
}
