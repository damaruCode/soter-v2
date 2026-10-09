use crate::{
    abstraction::Abstraction,
    analyzer::{dependency_checker::push_to_value_store, match_helper::MatchHelper},
    ast::{Case, Clause, Index, TypedCore, ValueAddressOrValue},
    state_space::{
        Closure, FailureType, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, Value,
        ValueAddress,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_case<K: KontinuationAddress, V: ValueAddress>(
    case: &Case,
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &dyn Abstraction<K, V>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();

    let clauses: Vec<Clause> = Vec::from(&case.clauses);

    let mats = match &*case.arg {
        TypedCore::Var(v) => {
            let v_addr = proc_state.env.inner.get(v.var_id.unwrap()).unwrap();
            MatchHelper::cs_match_vaddr(&clauses, v_addr, &store.value, ast_helper)
        }
        tc => {
            let value = Value::Closure(Closure {
                prog_loc: tc.get_index().unwrap(),
                env: proc_state.env.clone(),
            });
            MatchHelper::cs_match_value(&clauses, &value, &store.value, ast_helper)
        }
    };

    if mats.is_empty() {
        result.new.push((
            proc_state.fail(FailureType::Erlang("No matches.".to_string())),
            "abs_case".to_string(),
        ));
        return result;
    }

    for (index, substs) in mats {
        let mut new_item = proc_state.clone();

        new_item.prog_loc_or_pid =
            ProgLocOrPid::ProgLoc((*(clauses[index].body)).get_index().unwrap());

        for subst in substs {
            for (var_id, addr_or_value) in &subst.inner {
                match addr_or_value {
                    ValueAddressOrValue::Value(v) => {
                        // produce new v_addr, add to env and push into value store
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
                            v.clone(),
                        ) {
                            result.revisit.push((state, "abs_case".to_string()));
                        }
                    }
                    ValueAddressOrValue::ValueAddress(v) => {
                        // just push into env, because v is already in the value store
                        new_item.env.inner.insert(*var_id, v.clone());
                    }
                }
            }
        }

        result.new.push((new_item, "abs_case".to_string()));
    }

    result
}
