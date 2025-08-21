use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::push_to_kont_store,
    ast::{Index, Seq},
    state_space::{Kont, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, ValueAddress},
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_push_seq<K: KontinuationAddress, V: ValueAddress>(
    seq: &Seq,
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    let mut new_item = proc_state.clone();
    new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc((*seq.arg).get_index().unwrap());

    let new_kont = Kont::Seq(
        (*seq.body).get_index().unwrap(),
        proc_state.env.clone(),
        proc_state.k_addr.clone(),
    );

    let new_k_addr = abstraction.new_kaddr(
        &proc_state,
        &new_item.prog_loc_or_pid,
        &new_item.env,
        &new_item.time,
    );

    v_new.push((new_item, "abs_push_seq".to_string()));

    for state in push_to_kont_store(ast_helper, seen_proc_states, store, new_k_addr, new_kont) {
        v_revisit.push((state, "abs_push_seq".to_string()));
    }

    (v_new, v_revisit)
}
