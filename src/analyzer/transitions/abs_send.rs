use crate::{
    analyzer::dependency_checker::push_to_mailboxes,
    ast::{Index, TypedCore},
    state_space::{
        Closure, FailureType, KontinuationAddress, Mailboxes, Pid, ProcState, ProgLocOrPid, Store,
        Value, ValueAddress,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

fn resolve_pid<K: KontinuationAddress, V: ValueAddress>(
    value: &Value<V>,
    store: &Store<K, V>,
    ast_helper: &AstHelper,
) -> Vec<Pid> {
    let mut pids = Vec::new();
    match value {
        Value::Pid(pid) => pids.push(pid.clone()),
        Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
            TypedCore::Var(v) => {
                let var_id = v.var_id.unwrap();
                for value in store.unpack(&clo.env, var_id) {
                    pids.append(&mut resolve_pid(value, store, ast_helper));
                }
            }
            _ => panic!(), // TODO adapt to return an erronous result
        },
    };

    pids
}

pub fn abs_send<K: KontinuationAddress, V: ValueAddress>(
    typed_core_to: &TypedCore,
    typed_core_msg: &TypedCore,
    proc_state: &ProcState<K, V>,
    mailboxes: &mut Mailboxes<V>,
    store: &Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();

    let pids = match typed_core_to {
        TypedCore::Var(v) => {
            let var_id = v.var_id.unwrap();

            let mut pids = Vec::new();
            for maybe_pid in store.unpack(&proc_state.env, var_id) {
                pids.append(&mut resolve_pid(maybe_pid, store, ast_helper));
            }
            pids
        }
        tc => {
            result.new.push((
                proc_state.fail(FailureType::Unexpected(format!(
                    "Expected a variable, found {tc}"
                ))),
                "abs_send".to_string(),
            ));
            return result;
        }
    };

    fn determine_values<K: KontinuationAddress, V: ValueAddress>(
        typed_core_msg: &TypedCore,
        proc_state: &ProcState<K, V>,
        store: &Store<K, V>,
    ) -> Vec<Value<V>> {
        match typed_core_msg {
            TypedCore::Var(v) => store.unpack(&proc_state.env, v.var_id.unwrap()).clone(),
            TypedCore::Literal(_) | TypedCore::Cons(_) | TypedCore::Tuple(_) => {
                Vec::from([Value::Closure(Closure {
                    prog_loc: typed_core_msg.get_index().unwrap(),
                    env: proc_state.env.clone(),
                })])
            }
            tc => panic!("{tc:#?}"), // TODO adapt to return erronous result
        }
    }

    let msg_values = determine_values(typed_core_msg, proc_state, store);
    for pid in pids {
        for value in &msg_values {
            let mut new_item = proc_state.clone();

            match value {
                Value::Pid(pid) => new_item.prog_loc_or_pid = ProgLocOrPid::Pid(pid.clone()),
                Value::Closure(clo) => {
                    new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(clo.prog_loc);
                    new_item.env = clo.env.clone();
                }
            }

            result.new.push((new_item, "abs_send".to_string()));

            for state in push_to_mailboxes(
                ast_helper,
                seen_proc_states,
                mailboxes,
                pid.clone(),
                value.clone(),
            ) {
                result.revisit.push((state, "abs_send".to_string()));
            }
        }
    }

    result
}
