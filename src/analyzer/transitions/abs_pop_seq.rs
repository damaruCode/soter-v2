use crate::state_space::{
    Env, KontinuationAddress, ProcState, ProgLoc, ProgLocOrPid, ValueAddress,
};

use super::TransitionResult;

pub fn abs_pop_seq<K: KontinuationAddress, V: ValueAddress>(
    proc_state: &ProcState<K, V>,
    kont_prog_loc: ProgLoc,
    kont_env: &Env<V>,
    kont_k_addr: &K,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();

    let mut new_item = proc_state.clone();
    new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(kont_prog_loc.clone());
    new_item.env = kont_env.clone();
    new_item.k_addr = kont_k_addr.clone();

    v_new.push(new_item);

    (v_new, Vec::new())
}
