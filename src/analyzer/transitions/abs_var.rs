use crate::{
    ast::Var,
    state_space::{KontinuationAddress, ProcState, ProgLocOrPid, Store, Value, ValueAddress},
};

use super::TransitionResult;

pub fn abs_name<K: KontinuationAddress, V: ValueAddress>(
    var: &Var,
    proc_state: &ProcState<K, V>,
    store: &Store<K, V>,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();

    let var_id = var.var_id.unwrap();
    let values = store.unpack(&proc_state.env, var_id);

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

    result
}
