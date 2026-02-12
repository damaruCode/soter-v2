use crate::{
    abstraction::Abstraction,
    analyzer::{dependency_checker::push_to_value_store, match_helper::MatchHelper},
    ast::{Case, Clause, Index, MaybeIndex, TypedCore},
    state_space::{
        FailureType, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, ValueAddress,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_case<K: KontinuationAddress, V: ValueAddress>(
    case: &Case,
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();

    let clauses: Vec<Clause> = Vec::from(&case.clauses);
    let v_addr;
    match &*case.arg {
        TypedCore::Var(v) => match &v.var_id {
            MaybeIndex::Some(var_id) => v_addr = proc_state.env.inner.get(var_id).unwrap(),
            MaybeIndex::None => {
                result.new.push((
                    proc_state.fail(FailureType::Unexpected(format!(
                        "Found variable without var id: {}",
                        v
                    ))),
                    "abs_case".to_string(),
                ));

                return result;
            }
        },
        TypedCore::Values(v) => {
            if v.es.inner.len() == 0 {
                // empty case
                for clause in clauses {
                    // empty clause
                    if clause.pats.inner.len() == 0 {
                        let mut new_item = proc_state.clone();
                        new_item.prog_loc_or_pid =
                            ProgLocOrPid::ProgLoc((*clause.body).get_index().unwrap());

                        result.new.push((new_item, "abs_case".to_string()));

                        return result;
                    }
                }
            }

            result.new.push((
                proc_state.fail(FailureType::NotImplemented(
                    "Can not handle values of length higher than 0.".to_string(),
                )),
                "abs_case".to_string(),
            ));
            return result;
        }
        TypedCore::Literal(_) => {
            // TODO implement
            result.new.push((
                proc_state.fail(FailureType::NotImplemented(
                    "Can not handle literals as case arguments.".to_string(),
                )),
                "abs_case".to_string(),
            ));

            return result;
        }
        TypedCore::AstTuple(_) => {
            // TODO implement
            result.new.push((
                proc_state.fail(FailureType::NotImplemented(
                    "Can not handle tuples as case arguments.".to_string(),
                )),
                "abs_case".to_string(),
            ));

            return result;
        }
        tc => {
            // TODO possibly add more context, i.e. the choice of case that lead to this failure
            result.new.push((
                proc_state.fail(FailureType::Erlang(format!(
                    "Invalid case argument: {}",
                    tc
                ))),
                "abs_case".to_string(),
            ));

            return result;
        }
    }

    let mats = MatchHelper::vmatch(&clauses, v_addr, &store.value, ast_helper);

    for (_, matches) in mats {
        if matches.len() == 0 {
            result.new.push((
                proc_state.fail(FailureType::Erlang("No matches.".to_string())),
                "abs_case".to_string(),
            ));
            continue;
        }
        // only consider first match
        let (index, substs) = &matches[0];

        let mut new_item = proc_state.clone();
        new_item.prog_loc_or_pid =
            ProgLocOrPid::ProgLoc((*(clauses[*index].body)).get_index().unwrap());

        for i in 0..substs.len() {
            for (maybe_var_id, value) in &substs[i].inner {
                match maybe_var_id {
                    MaybeIndex::Some(var_id) => {
                        let new_v_addr = abstraction.new_vaddr(
                            proc_state,
                            *var_id,
                            &new_item.prog_loc_or_pid,
                            &new_item.env,
                            &new_item.time,
                        );
                        new_item.env.inner.insert(*var_id, new_v_addr.clone());

                        for state in push_to_value_store(
                            ast_helper,
                            seen_proc_states,
                            store,
                            new_v_addr,
                            value.clone(),
                        ) {
                            result.revisit.push((state, "abs_case".to_string()));
                        }
                    }
                    MaybeIndex::None => {
                        result.new.push((
                            proc_state.fail(FailureType::Unexpected(format!(
                                "Substitution has var without proper var id.",
                            ))),
                            "abs_case".to_string(),
                        ));
                        continue;
                    }
                }
            }
        }

        result.new.push((new_item, "abs_case".to_string()));
    }

    result
}
