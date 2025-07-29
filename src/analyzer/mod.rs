use crate::state_space::r#abstract::ProcState;
use crate::transition_system::{TransitionError, TransitionSystem, TypedTransition};

// pub const PROC_TRANSITION_SYSTEM: TransitionSystem<
//     ProcState<'static>,
//     fn(&ProcState) -> Result<ProcState<'static>, TransitionError>,
// > = TransitionSystem::init();

pub struct Analyzer<'a> {
    transition_system: TransitionSystem<
        ProcState<'a>,
        fn(&ProcState<'a>) -> Result<ProcState<'a>, TransitionError>,
    >,
}

impl<'a> Analyzer<'a> {
    pub fn new() -> Self {
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
            transition_system: proc_transition_system,
        }
    }

    pub fn apply(&self, state: &'a ProcState) -> Result<ProcState, TransitionError> {
        self.transition_system.try_apply(state)
    }
}
