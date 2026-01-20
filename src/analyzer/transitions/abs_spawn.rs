use crate::{
    abstraction::Abstraction,
    ast::{Index, TypedCore},
    state_space::{
        Env, FailureType, KontinuationAddress, Mailbox, Mailboxes, Pid, ProcState, ProgLocOrPid,
        Store, Time, Value, ValueAddress, VarName,
    },
    util::AstHelper,
};

use super::TransitionResult;

pub fn abs_spawn<K: KontinuationAddress, V: ValueAddress>(
    var_name: &VarName,
    proc_state: &ProcState<K, V>,
    mailboxes: &mut Mailboxes<V>,
    store: &Store<K, V>,
    module_env: &Env<V>,
    ast_helper: &AstHelper,
    abstraction: &Box<dyn Abstraction<K, V>>,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();

    let values = store
        .value
        .get(proc_state.env.inner.get(&var_name).unwrap())
        .unwrap();

    for value in values {
        match value {
            Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                TypedCore::Fun(f) => {
                    if f.vars.inner.len() != 0 {
                        result.new.push((
                            proc_state.fail(FailureType::General),
                            "abs_spawn".to_string(),
                        ));
                        continue;
                    }
                    let mut new_time =
                        abstraction.tick(&proc_state.pid.time, proc_state.pid.prog_loc);
                    new_time.append(proc_state.time.inner.clone());
                    let new_pid = Pid {
                        prog_loc: match proc_state.prog_loc_or_pid {
                            ProgLocOrPid::ProgLoc(pl) => pl,
                            _ => {
                                result.new.push((
                                    proc_state.fail(FailureType::General),
                                    "abs_spawn".to_string(),
                                ));
                                return result;
                            }
                        },
                        time: new_time,
                    };

                    let mut new_proc_state_one = proc_state.clone();
                    new_proc_state_one.prog_loc_or_pid = ProgLocOrPid::Pid(new_pid.clone());

                    result
                        .new
                        .push((new_proc_state_one, "abs_spawn".to_string()));

                    match &*f.body {
                        TypedCore::Case(c) => match &c.clauses.inner[0] {
                            TypedCore::Clause(c) => {
                                let mut new_proc_state_two = ProcState::new(
                                    new_pid.clone(),
                                    ProgLocOrPid::ProgLoc((*c.body).get_index().unwrap()),
                                    clo.env.clone(),
                                    abstraction.stop_kaddr(),
                                    Time::init(),
                                );
                                for (var_name, v_addr) in &module_env.inner {
                                    new_proc_state_two
                                        .env
                                        .inner
                                        .insert(var_name.clone(), v_addr.clone());
                                }

                                result
                                    .new
                                    .push((new_proc_state_two, "abs_spawn".to_string()));

                                mailboxes.inner.insert(new_pid, Mailbox::init());
                            }
                            _ => {
                                result.new.push((
                                    proc_state.fail(FailureType::General),
                                    "abs_call".to_string(),
                                ));
                            }
                        },
                        _ => {
                            result.new.push((
                                proc_state.fail(FailureType::General),
                                "abs_call".to_string(),
                            ));
                        }
                    }
                }
                _ => {
                    result.new.push((
                        proc_state.fail(FailureType::General),
                        "abs_call".to_string(),
                    ));
                }
            },
            _ =>
            // NOTE this should probably also be a failstate not a panic
            {
                result.new.push((
                    proc_state.fail(FailureType::General),
                    "abs_spawn".to_string(),
                ));
                // panic!(
                //     "Expected a closure, got Pid: {:?}\nFor variable: {:?}",
                //     value, var_name
                // )
            }
        }
    }

    result
}
