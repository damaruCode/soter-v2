use crate::{
    ast::Var,
    state_space::{
        KontinuationAddress, ProcState, ProgLocOrPid, Store, Value, ValueAddress, VarName,
    },
};

use super::TransitionResult;

pub fn abs_name<K: KontinuationAddress, V: ValueAddress>(
    var: &Var,
    proc_state: &ProcState<K, V>,
    store: &Store<K, V>,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();

    match proc_state.env.inner.get(&VarName::from(&*var.name)) {
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
            None => panic!("VAddr does not exist within value store"),
        },
        None => panic!("No VAddr exists for given Var"),
    };

    result
}
