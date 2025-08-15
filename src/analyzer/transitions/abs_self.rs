use crate::state_space::r#abstract::{KontinuationAddress, ProcState, ProgLocOrPid, ValueAddress};

use super::TransitionResult;

pub fn abs_self<K: KontinuationAddress, V: ValueAddress>(
    proc_state: &ProcState<K, V>,
) -> TransitionResult<K, V> {
    let mut new_item = proc_state.clone();
    new_item.prog_loc_or_pid = ProgLocOrPid::Pid(proc_state.pid.clone());

    (Vec::from([new_item]), Vec::new())
}
