use crate::{
    ast::PrimOp,
    state_space::{FailureType, KontinuationAddress, ProcState, ValueAddress},
};

use super::TransitionResult;

pub fn abs_primop<K: KontinuationAddress, V: ValueAddress>(
    prim_op: &PrimOp,
    proc_state: &ProcState<K, V>,
) -> TransitionResult<K, V> {
    // todo!("ABS_PRIMOP, ABS_SELF, ABS_SPAWN, ABS_SEND, {:#?}", _prim_op)
    TransitionResult {
        new: Vec::from([(
            proc_state.fail(FailureType::NotImplemented(format!(
                "ABS_PRIMOP {:#?}",
                prim_op
            ))),
            "abs_primop".to_string(),
        )]),
        revisit: Vec::new(),
    }
}
