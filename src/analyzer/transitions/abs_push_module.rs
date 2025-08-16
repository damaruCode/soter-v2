use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::push_to_kont_store,
    ast::{Index, Module},
    state_space::{Kont, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, ValueAddress},
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_push_module<K: KontinuationAddress, V: ValueAddress>(
    module: &Module,
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let v_revisit;

    let mut new_item = proc_state.clone();
    new_item.prog_loc_or_pid =
        ProgLocOrPid::ProgLoc((*module.defs.inner[0].scnd).get_index().unwrap()); // straight to the fun

    let kont = Kont::Module(0, proc_state.env.clone(), proc_state.k_addr.clone());
    let new_k_addr = abstraction.new_kaddr(
        proc_state,
        &new_item.prog_loc_or_pid,
        &new_item.env,
        &new_item.time,
    );

    new_item.k_addr = new_k_addr.clone();
    log::debug!(
        "PUSH_MODULE {:#?}",
        ast_helper.get(match new_item.prog_loc_or_pid {
            ProgLocOrPid::ProgLoc(pl) => pl,
            _ => panic!(),
        })
    );
    v_new.push(new_item);

    v_revisit = push_to_kont_store(ast_helper, seen_proc_states, store, new_k_addr, kont);

    log::debug!(
        "ABS_PUSH_MODULE - {:?} New - {:?} Revisit",
        v_new.len(),
        v_revisit.len()
    );
    (v_new, v_revisit)
}
