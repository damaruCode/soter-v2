use crate::{
    ast::{Case, Index, TypedCore, ValueAddressOrValue, VarNamesOrValue},
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
    let v_addr;
    match &*case.arg {
        TypedCore::Var(v) => v_addr = proc_state.env.inner.get(&VarName::from(v)).unwrap(),
        TypedCore::Literal(l) => match *l.val.clone() {
            TypedCore::AstList(al) => todo!("{:#?}", al),
            TypedCore::String(s) => {
                todo!("{:#?}", s);
            }
            _ => panic!(),
        },
        TypedCore::AstTuple(t) => todo!("{:#?}", t),
        _ => panic!(),
    }

    let mats = Case::cmatch(
        &clauses,
        &ValueAddressOrValue::ValueAddress(v_addr.clone()),
        &store.value,
        ast_helper,
    );

    for opt in mats {
        if let Some((index, env)) = opt {
            let mut new_item = proc_state.clone();
            new_item.prog_loc_or_pid =
                ProgLocOrPid::ProgLoc((*(clauses[index].body)).get_index().unwrap());
            new_item.env.merge_with(&env);

            v_new.push(new_item);
        }
    }

    (v_new, Vec::new())
}
