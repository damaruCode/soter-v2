use crate::state_space::{KontinuationAddress, ProcState, ProgLocOrPid, ValueAddress};

use super::TransitionResult;

pub fn abs_self<K: KontinuationAddress, V: ValueAddress>(
    proc_state: &ProcState<K, V>,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();

    let mut new_item = proc_state.clone();
    new_item.prog_loc_or_pid = ProgLocOrPid::Pid(proc_state.pid.clone());

    result.new.push((new_item, "abs_self".to_string()));
    result
}
