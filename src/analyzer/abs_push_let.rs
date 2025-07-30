use crate::{
    ast::TypedCore,
    state_space::r#abstract::{KAddr, Kont, ProcState, ProgLoc, ProgLocOrPid},
    transition_system::{Transition, TransitionError},
};

use super::TransitionState;

pub struct AbsPushLet<'a> {
    transition_fn: fn(&TransitionState<'a>) -> Result<TransitionState<'a>, TransitionError>,
}
impl<'a> AbsPushLet<'a> {
    pub fn new() -> Self {
        Self {
            transition_fn: |transition_state| {
                let mut new_program_state = transition_state.program_state.clone();

                if let ProgLocOrPid::ProgLoc(ref prog_loc) =
                    transition_state.process_state.prog_loc_or_pid
                {
                    if let TypedCore::Let(l) = prog_loc.get() {
                        let var_list = &l.vars;
                        let arg = &l.arg;
                        let body = &l.body;

                        // Push-Let
                        let k_let = Kont::Let(
                            var_list,
                            ProgLoc::new(body),
                            transition_state.process_state.env.clone(),
                            transition_state.process_state.k_addr.clone(),
                        );

                        let k_addr = KAddr::new(
                            transition_state.process_state.pid.clone(),
                            prog_loc.clone(),
                            transition_state.process_state.env.clone(),
                            transition_state.process_state.time.clone(),
                        );

                        new_program_state.store.kont.push(k_addr.clone(), k_let);
                        let new_process_state = ProcState::new(
                            transition_state.process_state.pid.clone(),
                            ProgLocOrPid::ProgLoc(ProgLoc::new(arg)),
                            transition_state.process_state.env.clone(),
                            k_addr,
                            transition_state.process_state.time.clone(),
                        );

                        return Ok(TransitionState {
                            process_state: new_process_state,
                            program_state: new_program_state,
                        });
                    }
                }

                Err(TransitionError::ErroneousTransition)
            },
        }
    }
}
impl<'a> Transition<TransitionState<'a>> for AbsPushLet<'a> {
    type Error = TransitionError;

    fn apply(&self, s: &TransitionState<'a>) -> Result<TransitionState<'a>, Self::Error> {
        (self.transition_fn)(s)
    }

    fn is_valid(&self, s: &TransitionState<'a>) -> bool {
        self.apply(s).is_ok()
    }
}
