use crate::{
    ast::{Index, Receive},
    state_space::{
        Env, KontinuationAddress, Mailboxes, ProcState, ProgLocOrPid, Store, ValueAddress,
    },
    util::AstHelper,
};

use super::TransitionResult;

pub fn abs_receive<K: KontinuationAddress, V: ValueAddress>(
    receive: &Receive,
    proc_state: &ProcState<K, V>,
    mailboxes: &Mailboxes<V>,
    store: &Store<K, V>,
    module_env: &Env<V>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();

    let mailbox = mailboxes.inner.get(&proc_state.pid).unwrap();
    let clauses = &Vec::from(&receive.clauses);
    let matching_msgs = mailbox.mmatch(clauses, &store.value, module_env, ast_helper);

    // NOTE the Mailbox_set abstraction does not extract any messages, so finding
    // the matched message is simply a case of looking at the message at `index`
    for (index, env) in matching_msgs {
        let mut new_item = proc_state.clone();
        new_item.prog_loc_or_pid =
            ProgLocOrPid::ProgLoc((*clauses[index].body).get_index().unwrap());
        println!("{:#?} - {:#?}", clauses[index], env);
        new_item.env.merge_with(&env);

        v_new.push(new_item);
    }
    log::debug!("ABS_RECEIVE - {:?} New - {:?} Revisit", v_new.len(), 0);
    (v_new, Vec::new())
}
