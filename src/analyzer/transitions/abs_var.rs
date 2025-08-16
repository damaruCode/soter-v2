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
    let mut v_new = Vec::new();

    match proc_state.env.inner.get(&VarName::from(&*var.name)) {
        Some(v) => match store.value.get(&v) {
            Some(values) => {
                for value in values {
                    // consider each
                    // non-deterministically
                    match value {
                        Value::Closure(c) => {
                            let mut new_item = proc_state.clone();
                            new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(c.prog_loc);
                            v_new.push(new_item);
                        }
                        Value::Pid(_) => {
                            panic!("Unexpected value: Expected Closure not Pid")
                        }
                    }
                }
            }
            None => panic!("VAddr does not exist within value store"),
        },
        None => panic!("No VAddr exists for given Var"),
    };

    (v_new, Vec::new())
}
