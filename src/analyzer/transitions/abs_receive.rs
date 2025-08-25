use crate::{
    ast::{Index, Receive},
    state_space::{KontinuationAddress, Mailboxes, ProcState, ProgLocOrPid, Store, ValueAddress},
    util::AstHelper,
};

use super::TransitionResult;

pub fn abs_receive<K: KontinuationAddress, V: ValueAddress>(
    receive: &Receive,
    proc_state: &ProcState<K, V>,
    mailboxes: &Mailboxes<V>,
    store: &mut Store<K, V>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();

    let mailbox = mailboxes.inner.get(&proc_state.pid).unwrap();
    let clauses = &Vec::from(&receive.clauses);
    let matching_msgs = mailbox.mmatch(clauses, &store.value, ast_helper);

    // NOTE the Mailbox_set abstraction does not extract any messages, so finding
    // the matched message is simply a case of looking at the message at `index`
    for (index, substs) in matching_msgs {
        let mut new_item = proc_state.clone();
        new_item.prog_loc_or_pid =
            ProgLocOrPid::ProgLoc((*clauses[index].body).get_index().unwrap());

        // introduce substitution into environment
        let mut new_env = proc_state.env.clone();
        for i in 0..substs.len() {
            for (var_name, v_addr) in &substs[i].inner {
                // NOTE because we use Data_0, the preliminary step of resolving the data d_i is
                // irrelevant --- it would only have been of interest for the VAddr
                new_env.inner.insert(var_name.clone(), v_addr.clone());
            }
        }
        new_item.env = new_env;

        v_new.push((new_item, "abs_receive".to_string()));
    }
    log::debug!("ABS_RECEIVE - {:?} New - {:?} Revisit", v_new.len(), 0);
    (v_new, Vec::new())
}
