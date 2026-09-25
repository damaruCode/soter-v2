use crate::{
    abstraction::Abstraction,
    analyzer::{dependency_checker::push_to_value_store, transitions::abs_pop_let_closure},
    ast::{MaybeIndex, TypedCore},
    state_space::{
        Closure, Env, FailureType, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, Value,
        ValueAddress,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_pop_let_value_list<K: KontinuationAddress, V: ValueAddress>(
    proc_state: &ProcState<K, V>,
    kont_var_list: &[usize],
    kont_body_prog_loc: usize,
    kont_env: &Env<V>,
    kont_k_addr: &K,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &dyn Abstraction<K, V>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();
    if kont_var_list.is_empty() {
        result.new.push((
            proc_state.fail(FailureType::Unexpected(
                "Expected var list of atleast 1, found length 0".to_string(),
            )),
            "abs_pop_let_value_list".to_string(),
        ));
        return result;
    }

    let mut result = TransitionResult::new();

    let mut new_item = proc_state.clone();
    new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(kont_body_prog_loc);
    new_item.env = kont_env.clone();
    new_item.k_addr = kont_k_addr.clone();

    // get the value_list from ProcState prog_loc
    let value_list;
    let value_list_pl;
    match &proc_state.prog_loc_or_pid {
        ProgLocOrPid::ProgLoc(pl) => {
            value_list_pl = pl;

            match ast_helper.get(*pl) {
                TypedCore::Tuple(tup) => value_list = tup.es.inner.clone(),
                TypedCore::AstList(al) => value_list = al.inner.clone(),
                tc => {
                    result.new.push((
                        proc_state.fail(FailureType::Unexpected(format!(
                            "Expected a value list but found : {tc}."
                        ))),
                        "abs_pop_let_value_list".to_string(),
                    ));
                    return result;
                }
            };
        }
        ProgLocOrPid::Pid(pid) => {
            result.new.push((
                proc_state.fail(FailureType::Unexpected(format!(
                    "Expected a value list but found a pid: {pid}."
                ))),
                "abs_pop_let_value_list".to_string(),
            ));
            return result;
        }
    }

    // check list length
    if kont_var_list.len() != value_list.len() {
        if kont_var_list.len() == 1 {
            // should instead do the abs_pop_let_closure transition
            return abs_pop_let_closure(
                proc_state,
                *value_list_pl,
                kont_var_list,
                kont_body_prog_loc,
                kont_env,
                kont_k_addr,
                store,
                seen_proc_states,
                abstraction,
                ast_helper,
            );
        }

        result.new.push((
            proc_state.fail(FailureType::Unexpected(format!(
                "Expected a value list of length {} but found a value list with length {}.",
                kont_var_list.len(),
                value_list.len()
            ))),
            "abs_pop_let_value_list".to_string(),
        ));
        return result;
    }

    // for each element in the kontinutation value list, create neccessary bindings
    for i in 0..kont_var_list.len() {
        match ast_helper.get(kont_var_list[i]) {
            TypedCore::Var(lvalue_var) => match &lvalue_var.var_id {
                MaybeIndex::Some(lvalue_var_id) => {
                    match &value_list[i] {
                        TypedCore::Var(rvalue_var) => match &rvalue_var.var_id {
                            MaybeIndex::Some(rvalue_var_id) => {
                                // if the rvalue is a variable, simply remap the lvalue binding in the env to
                                // the v_addr of the rvalue
                                new_item.env.inner.insert(
                                    *lvalue_var_id,
                                    proc_state.env.inner.get(rvalue_var_id).unwrap().clone(),
                                );
                            }
                            MaybeIndex::None => result.new.push((
                                proc_state.fail(FailureType::Unexpected(format!(
                                    "Found variable without var id: {rvalue_var}"
                                ))),
                                "abs_pop_let_value_list".to_string(),
                            )),
                        },
                        _ => {
                            // for any other rvalue (literal value), create a new v_addr and push the literal value in the value store
                            let new_v_addr = abstraction.new_vaddr(
                                proc_state,
                                *lvalue_var_id,
                                &new_item.prog_loc_or_pid,
                                &new_item.env,
                                &new_item.time,
                            );
                            new_item
                                .env
                                .inner
                                .insert(*lvalue_var_id, new_v_addr.clone());

                            for state in push_to_value_store(
                                ast_helper,
                                seen_proc_states,
                                store,
                                new_v_addr,
                                Value::Closure(Closure {
                                    prog_loc: *value_list_pl,
                                    env: proc_state.env.clone(),
                                }),
                            ) {
                                result
                                    .revisit
                                    .push((state, "abs_pop_let_value_list".to_string()));
                            }
                        }
                    };
                }
                MaybeIndex::None => result.new.push((
                    proc_state.fail(FailureType::Unexpected(format!(
                        "Found variable without var id: {lvalue_var}"
                    ))),
                    "abs_pop_let_value_list".to_string(),
                )),
            },
            tc => result.new.push((
                proc_state.fail(FailureType::Unexpected(format!(
                    "Expected a variable, found {tc}"
                ))),
                "abs_pop_let_value_list".to_string(),
            )),
        }
    }

    result
        .new
        .push((new_item, "abs_pop_let_value_list".to_string()));

    result
}
