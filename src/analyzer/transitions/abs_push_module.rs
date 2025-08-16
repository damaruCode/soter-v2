use crate::{
    analyzer::dependency_checker::push_to_kont_store,
    ast::{Index, Module},
    state_space::r#abstract::{
        AddressBuilder, Kont, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store,
        ValueAddress,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_push_module<K: KontinuationAddress, V: ValueAddress>(
    module: &Module,
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    address_builder: &Box<dyn AddressBuilder<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();

    let mut new_item = proc_state.clone();
    new_item.prog_loc_or_pid =
        ProgLocOrPid::ProgLoc((*module.defs.inner[0].scnd).get_index().unwrap()); // straight to the fun

    let kont = Kont::Module(0, proc_state.env.clone(), proc_state.k_addr.clone());
    let new_k_addr = address_builder.new_kaddr(
        proc_state,
        &new_item.prog_loc_or_pid,
        &new_item.env,
        &new_item.time,
    );

    new_item.k_addr = new_k_addr.clone();
    v_new.push(new_item);

    (
        v_new,
        push_to_kont_store(ast_helper, seen_proc_states, store, new_k_addr, kont),
    )
}
