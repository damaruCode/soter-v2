use crate::{
    ast::{Case, Index, TypedCore},
    state_space::r#abstract::{
        KontinuationAddress, ProcState, ProgLocOrPid, Store, ValueAddress, VarName,
    },
    util::AstHelper,
};

use super::TransitionResult;

pub fn abs_case<K: KontinuationAddress, V: ValueAddress>(
    case: &Case,
    proc_state: &ProcState<K, V>,
    store: &Store<K, V>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();

    let clauses = Vec::from(&case.clauses);
    let values;
    match &*case.arg {
        TypedCore::Var(v) => {
            values = store
                .value
                .get(&proc_state.env.inner.get(&VarName::from(v)).unwrap())
                .unwrap()
        }
        TypedCore::Literal(l) => match *l.val.clone() {
            TypedCore::AstList(al) => todo!("{:#?}", al),
            TypedCore::String(s) => {
                todo!("{:#?}", s);
                // let var_name = VarName::from(&*l.val);
            }
            _ => panic!(),
        },
        TypedCore::AstTuple(t) => todo!("{:#?}", t),
        _ => panic!(),
    }

    let mats = Case::cmatch(&clauses, values, &store.value, ast_helper);

    for mat in mats {
        if let Some((index, env)) = mat {
            let mut new_item = proc_state.clone();
            new_item.prog_loc_or_pid =
                ProgLocOrPid::ProgLoc((*(clauses[index].body)).get_index().unwrap());
            new_item.env.merge_with(&env);

            v_new.push(new_item);

            // stop after first match
            break;
        }
    }

    (v_new, Vec::new())
}
