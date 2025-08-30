use crate::{
    abstraction::Abstraction,
    analyzer::{dependency_checker::push_to_value_store, MatchHelper},
    ast::{Case, Clause, Index, TypedCore},
    state_space::{
        KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, ValueAddress, VarName,
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
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

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

                        v_new.push((new_item, "abs_case".to_string()));

                        return (v_new, Vec::new());
                    }
                }
            }

            todo!("{:#?}", v);
        }
        TypedCore::Literal(l) => match *l.val.clone() {
            TypedCore::AstList(al) => todo!("{:#?}", al),
            TypedCore::String(s) => {
                todo!("{:#?}", s);
            }
            _ => panic!(),
        },
        TypedCore::AstTuple(t) => todo!("{:#?}", t),
        tc => panic!("{:#?}", tc),
    }

    let mats = MatchHelper::vmatch(&clauses, v_addr, &store.value, ast_helper);

    for (_, matches) in mats {
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
                    v_revisit.push((state, "abs_case".to_string()));
                }
            }
        }
        v_new.push((new_item, "abs_case".to_string()));
    }

    log::debug!("ABS_CASE - {:?} New - {:?} Revisit", v_new.len(), 0);

    (v_new, v_revisit)
}
