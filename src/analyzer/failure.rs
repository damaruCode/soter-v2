use crate::state_space::{KontinuationAddress, ProcState, ValueAddress};

pub struct FailureContext<K: KontinuationAddress, V: ValueAddress> {
    pub proc_state: ProcState<K, V>,
    pub msg: String,
}
