use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::push_to_value_store,
    ast::{Apply, Index, TypedCore},
    state_space::{
        Closure, Env, FailureType, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, Value,
        ValueAddress,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_apply<K: KontinuationAddress, V: ValueAddress>(
    apply: &Apply,
    prog_loc_proc_state: usize,
    proc_state: &ProcState<K, V>,
    module_env: &Env<V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    store: &mut Store<K, V>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();

    match &*apply.op.clone() {
        TypedCore::Var(v) => {
            // need to clone for mutable borrow of store later on
            let op_values = store.unpack(&proc_state.env, v.var_id.unwrap()).clone(); 

            for op_value in op_values {
                match op_value {
                    Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                        TypedCore::Fun(f) => {
                            let mut fn_var_names = Vec::new();
                            for tc in &f.vars.inner {
                                match tc {
                                    TypedCore::Var(v) => fn_var_names.push(v.var_id.unwrap()),
                                    _ => {
                                        let fail_state =
                                            proc_state.fail(FailureType::Unexpected(format!(
                                                "Expected a formal parameter variable, found {}",
                                                tc
                                            )));
                                        result.new.push((fail_state, "abs_apply".to_string()));
                                        continue;
                                    }
                                }
                            }

                            let mut new_item = proc_state.clone();
                            new_item.prog_loc_or_pid =
                                ProgLocOrPid::ProgLoc((*f.body).get_index().unwrap());
                            new_item.time = abstraction.tick(&new_item.time, prog_loc_proc_state);

                            new_item.env = clo.env.clone();
                            new_item.env.merge_with(module_env);

                            if fn_var_names.len() != apply.args.inner.len() {
                                let fail_state = proc_state.fail(FailureType::Erlang(format!(
                                    "Expected {} arguments, got {}",
                                    fn_var_names.len(),
                                    apply.args.inner.len()
                                )));
                                result.new.push((fail_state, "abs_apply".to_string()));
                                continue;
                            }

                            if fn_var_names.len() > 0 {
                                for i in 0..fn_var_names.len() {
                                    // check the type of the arg
                                    match &apply.args.inner[i] {
                                        // for vars, we simply add a binding to the existent v_addr
                                        TypedCore::Var(v) => {
                                            
                                                new_item.env.inner.insert(
                                                    fn_var_names[i].clone(),
                                                    proc_state
                                                        .env
                                                        .inner
                                                        .get(v.var_id.unwrap())
                                                        .unwrap()
                                                        .clone(),
                                                );
                                        }
                                        TypedCore::Literal(_) // NOTE could handle this with a constant
                                                              // address
                                        | TypedCore::Cons(_)
                                        | TypedCore::Tuple(_)
                                        | TypedCore::Fun(_) => {
                                            let new_v_addr = abstraction.new_vaddr(
                                                proc_state,
                                                *fn_var_names[i],
                                                &new_item.prog_loc_or_pid,
                                                &new_item.env,
                                                &new_item.time,
                                            );

                                            new_item
                                                .env
                                                .inner
                                                .insert(fn_var_names[i].clone(), new_v_addr.clone());

                                            for state in push_to_value_store(
                                                ast_helper,
                                                seen_proc_states,
                                                store,
                                                new_v_addr,
                                                Value::Closure(Closure {
                                                    prog_loc: apply.args.inner[i].get_index().unwrap(),
                                                    env: proc_state.env.clone(),
                                                }),
                                            ) {
                                                result.revisit.push((state, "abs_apply".to_string()));
                                            }
                                        }
                                        tc => {
                                            result.new.push((proc_state.fail(FailureType::NotImplemented(format!("No behaviour implemented for {}", tc))), "abs_apply".to_string()));
                                            continue;
                                        },
                                    }
                                }
                            }
                            result.new.push((new_item, "abs_apply".to_string()));
                        }
                        tc => {
                            result.new.push((
                                proc_state.fail(FailureType::Erlang(format!(
                                    "Function expected, found {}",
                                    tc
                                ))),
                                "abs_apply".to_string(),
                            ));
                            continue;
                        }
                    },
                    Value::Pid(pid) => {
                        result.new.push((
                            proc_state.fail(FailureType::Erlang(format!(
                                "Expected closure, found pid {}",
                                pid
                            ))),
                            "abs_apply".to_string(),
                        ));
                        continue;
                    }
                }
            }
        }
        tc => {
            result.new.push((
                proc_state.fail(FailureType::Erlang(format!(
                    "Expected variable name, found {}",
                    tc
                ))),
                "abs_apply".to_string(),
            ));
        }
    }

    return result;
}
