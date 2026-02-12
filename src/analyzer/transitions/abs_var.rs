use crate::{
    ast::{MaybeIndex, Var},
    state_space::{
        FailureType, KontinuationAddress, ProcState, ProgLocOrPid, Store, Value, ValueAddress,
    },
};

use super::TransitionResult;

pub fn abs_name<K: KontinuationAddress, V: ValueAddress>(
    var: &Var,
    proc_state: &ProcState<K, V>,
    store: &Store<K, V>,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();

    match &var.var_id {
        MaybeIndex::Some(var_id) => match proc_state.env.inner.get(var_id) {
            Some(v) => match store.value.get(&v) {
                Some(values) => {
                    for value in values {
                        let mut new_item = proc_state.clone();
                        match value {
                            Value::Closure(clo) => {
                                new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(clo.prog_loc);
                                new_item.env = clo.env.clone();
                            }
                            Value::Pid(pid) => {
                                new_item.prog_loc_or_pid = ProgLocOrPid::Pid(pid.clone());
                            }
                        }
                        result.new.push((new_item, "abs_var".to_string()));
                    }
                }
                None => result.new.push((
                    proc_state.fail(FailureType::Unexpected(format!(
                        "Expected value for {} in value store, found nothing.",
                        var_id
                    ))),
                    "abs_var".to_string(),
                )),
            },
            None => result.new.push((
                proc_state.fail(FailureType::Unexpected(format!(
                    "Expected v_addr for {} in environment, found nothing",
                    var_id
                ))),
                "abs_var".to_string(),
            )),
        },
        MaybeIndex::None => result.new.push((
            proc_state.fail(FailureType::Unexpected(format!(
                "Found variable without var id: {}",
                var
            ))),
            "abs_var".to_string(),
        )),
    };

    result
}
