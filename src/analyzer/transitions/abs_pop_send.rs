use crate::{
    analyzer::dependency_checker::push_to_mailboxes,
    state_space::{
        Env, KontinuationAddress, Mailboxes, Pid, ProcState, ProgLoc, ProgLocOrPid, ValueAddress,
        VarName,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_pop_send<K: KontinuationAddress, V: ValueAddress>(
    kont_msg_prog_loc: ProgLoc,
    kont_env: &Env<V>,
    kont_k_addr: &K,
    proc_state: &ProcState<K, V>,
    mailboxes: &mut Mailboxes<V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    // kont_msg_prog_loc should be a variable
    let msg_var_name = VarName::from(ast_helper.get(kont_msg_prog_loc));

    // proc_state.prog_loc_or_pid should be a pid now
    let pid = match &proc_state.prog_loc_or_pid {
        ProgLocOrPid::Pid(pid) => pid.clone(),
        _ => panic!(),
    };

    let mut new_item = proc_state.clone();
    new_item.prog_loc_or_pid = ProgLocOrPid::Pid(pid.clone());
    new_item.k_addr = kont_k_addr.clone();

    for state in push_to_mailboxes(
        ast_helper,
        seen_proc_states,
        mailboxes,
        pid.clone(),
        kont_env.inner.get(&msg_var_name).unwrap().clone(),
    ) {
        v_revisit.push((state, "abs_send".to_string()));
    }
    v_new.push((new_item, "abs_send".to_string()));

    log::debug!(
        "ABS_SEND - {:?} New - {:?} Revisit",
        v_new.len(),
        v_revisit.len()
    );
    (v_new, v_revisit)
}
