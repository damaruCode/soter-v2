use crate::{
    state_space::r#abstract::ProcState,
    transition_systems::transition_system::{TransitionError, TransitionSystem},
};

// pub const PROC_TRANSITION_SYSTEM: TransitionSystem<
//     ProcState<'static>,
//     fn(&ProcState) -> Result<ProcState<'static>, TransitionError>,
// > = TransitionSystem::init();

fn transition_system<'a>() {
    let proc_transition_system: TransitionSystem<
        ProcState<'a>,
        fn(&ProcState<'a>) -> Result<ProcState<'a>, TransitionError>,
    > = TransitionSystem::init();
}
