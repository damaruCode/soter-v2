use crate::{
    abstraction::Abstraction,
    analyzer::{dependency_checker::push_to_value_store, match_helper::MatchHelper},
    ast::{Case, Clause, Index, TypedCore},
    state_space::{
        FailureType, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, ValueAddress,
        VarName,
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
        TypedCore::Var(v) => v_addr = proc_state.env.inner.get(&VarName::from(v)).unwrap(),
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
                proc_state.fail(FailureType::NotImplemented),
                "abs_case".to_string(),
            ));
            return result;
        }
        TypedCore::Literal(l) => match *l.val.clone() {
            TypedCore::AstList(al) => todo!("{:#?}", al),
            TypedCore::String(_) => {
                // TODO implement
                let mut result = TransitionResult::new();
                result.new.push((
                    proc_state.fail(crate::state_space::FailureType::NotImplemented),
                    "abs_case".to_string(),
                ));

                return result;
            }
            _ => panic!(),
        },
        TypedCore::AstTuple(t) => todo!("{:#?}", t),
        tc => panic!("{:#?}", tc),
    }

    let mats = MatchHelper::vmatch(&clauses, v_addr, &store.value, ast_helper);

    for (_, matches) in mats {
        if matches.len() == 0 {
            result.new.push((
                proc_state.fail(FailureType::General),
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
            for (var_name, value) in &substs[i].inner {
                let new_v_addr = abstraction.new_vaddr(
                    proc_state,
                    var_name,
                    &new_item.prog_loc_or_pid,
                    &new_item.env,
                    &new_item.time,
                );
                new_item
                    .env
                    .inner
                    .insert(var_name.clone(), new_v_addr.clone());

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
        }

        result.new.push((new_item, "abs_case".to_string()));
    }

    result
}
