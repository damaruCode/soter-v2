use crate::ast::TypedCore;
use crate::state_space::r#abstract::{ProcState, State};
use crate::transition_system::{TransitionError, TransitionSystem, TypedTransition};

// pub const PROC_TRANSITION_SYSTEM: TransitionSystem<
//     ProcState<'static>,
//     fn(&ProcState) -> Result<ProcState<'static>, TransitionError>,
// > = TransitionSystem::init();

pub struct Analyzer<'a> {
    curr_state: State<'a>,
    proc_transition_system: TransitionSystem<
        ProcState<'a>,
        fn(&ProcState<'a>) -> Result<ProcState<'a>, TransitionError>,
    >,
}

impl<'a> Analyzer<'a> {
    pub fn new(ast: &'a TypedCore) -> Self {
        let mut proc_transition_system: TransitionSystem<
            ProcState<'a>,
            fn(&ProcState<'a>) -> Result<ProcState<'a>, TransitionError>,
        > = TransitionSystem::init();
        let basic_transition: TypedTransition<
            ProcState,
            fn(&ProcState<'a>) -> Result<ProcState<'a>, TransitionError>,
        > = TypedTransition::new(|s| Ok(s.clone()));
        proc_transition_system.register_transition(basic_transition);

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
}
