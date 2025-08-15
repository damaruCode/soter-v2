use crate::{
    ast::{Index, Module, TypedCore},
    state_space::r#abstract::{KontinuationAddress, ProcState, ProgLocOrPid, ValueAddress},
};

use super::TransitionResult;

pub fn abs_module<K: KontinuationAddress, V: ValueAddress>(
    proc_state: &ProcState<K, V>,
    module: &Module,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();

    match &*module.defs.inner[0].scnd {
        TypedCore::Fun(f) => match &*f.body {
            // NOTE could have more clauses than one
            TypedCore::Case(c) => match &c.clauses.inner[0] {
                TypedCore::Clause(c) => {
                    let mut new_item = proc_state.clone();
                    let index = (*c.body).get_index().unwrap();
                    new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(index);
                    v_new.push(new_item);
                }
                _ => panic!(),
            },
            _ => panic!(),
        },
        _ => panic!(),
    }

    (v_new, Vec::new())
}
