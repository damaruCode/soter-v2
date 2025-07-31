mod abs_push_let;

use abs_push_let::AbsPushLet;

use crate::state_space::r#abstract::{ProcState, SetMap, State};
use crate::transition_system::{TransitionError, TransitionSystem};

pub struct Analyzer<'a> {
    current_program_state: State<'a>,
    transition_system: TransitionSystem<
        TransitionState<'a>,
        fn(&TransitionState<'a>) -> Result<TransitionState<'a>, TransitionError>,
    >,
}
impl<'a> Analyzer<'a> {
    pub fn new(program_state: State<'static>) -> Self {
        let mut transition_system: TransitionSystem<
            TransitionState<'a>,
            fn(&TransitionState<'a>) -> Result<TransitionState<'a>, TransitionError>,
        > = TransitionSystem::init();

        // let t_push_let = AbsPushLet::new();
        // transition_system.register_transition(t_push_let);

        Analyzer {
            current_program_state: program_state,
            transition_system,
        }
    }

    pub fn step(&self) -> Result<State, TransitionError> {
        let mut next_program_state = self.current_program_state.clone();
        next_program_state.procs.inner = SetMap::init(); // clear the setmap; we will refill it
                                                         // with the new process states

        for (pid, proc_state) in &self.current_program_state.procs.inner {
            let transition_state =
                TransitionState::new(proc_state.clone(), next_program_state.clone());

            match self.transition_system.try_apply(&transition_state) {
                Ok(new_transition_state) => {
                    next_program_state = new_transition_state.program_state;
                    next_program_state
                        .procs
                        .inner
                        .push(pid.clone(), new_transition_state.process_state);
                }
                Err(e) => return Err(e),
            };
        }

        Ok(next_program_state)
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
    // ABS_POP_LET_CLOSURE
    // ABS_POP_LET_PID
    // ABS_POP_LET_VALUEADDR
    // ABS_POP_LET_VALUELIST
}

#[derive(Clone, Debug, PartialEq)]
pub struct TransitionState<'a> {
    pub process_state: ProcState<'a>,
    pub program_state: State<'a>,
}
impl<'a> TransitionState<'a> {
    fn new(process_state: ProcState<'a>, program_state: State<'a>) -> Self {
        TransitionState {
            process_state,
            program_state,
        }
    }
}
