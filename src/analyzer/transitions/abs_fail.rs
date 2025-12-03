use crate::{
    analyzer::failure::FailureContext,
    state_space::{KontinuationAddress, ProcState, ValueAddress},
};

use super::TransitionResult;

pub fn abs_fail<K: KontinuationAddress, V: ValueAddress>(
    failures: &mut Vec<FailureContext<K, V>>,
    proc_state: &ProcState<K, V>,
    msg: String,
) -> TransitionResult<K, V> {
    // construct an ErrorContext
    let ec = FailureContext {
        proc_state: proc_state.clone(),
        msg: msg,
    };

    // put it into an overall accumulator in the analyzer
    failures.push(ec);

    (Vec::new(), Vec::new())
}
