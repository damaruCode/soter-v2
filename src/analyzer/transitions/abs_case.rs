use crate::{
    ast::{Case, Clause, Index, TypedCore, ValueAddressOrValue},
    state_space::{
        Env, KontinuationAddress, ProcState, ProgLocOrPid, Store, ValueAddress, VarName,
    },
    util::AstHelper,
};

use super::TransitionResult;

pub fn abs_case<K: KontinuationAddress, V: ValueAddress>(
    case: &Case,
    proc_state: &ProcState<K, V>,
    store: &Store<K, V>,
    module_env: &Env<V>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();

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

    let mats = Case::cmatch(
        &clauses,
        &ValueAddressOrValue::ValueAddress(v_addr.clone()),
        &store.value,
        module_env,
        ast_helper,
    );

    for opt in mats {
        if let Some((index, env)) = opt {
            let mut new_item = proc_state.clone();
            new_item.prog_loc_or_pid =
                ProgLocOrPid::ProgLoc((*(clauses[index].body)).get_index().unwrap());
            new_item.env.merge_with(&env);

            v_new.push((new_item, "abs_case".to_string()));
        }
    }

    log::debug!("ABS_CASE - {:?} New - {:?} Revisit", v_new.len(), 0);

    (v_new, Vec::new())
}
