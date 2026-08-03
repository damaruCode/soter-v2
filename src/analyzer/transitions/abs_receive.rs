use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::push_to_value_store,
    ast::{Index, Receive, ValueAddressOrValue},
    state_space::{
        KontinuationAddress, Mailboxes, Pid, ProcState, ProgLocOrPid, Store, ValueAddress,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_receive<K: KontinuationAddress, V: ValueAddress>(
    receive: &Receive,
    proc_state: &ProcState<K, V>,
    mailboxes: &Mailboxes<V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();

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
        for i in 0..substs.len() {
            for (var_id, addr_or_value) in &substs[i].inner {
                // NOTE because we use Data_0, the preliminary step of resolving the data d_i is
                // irrelevant --- it would only have been of interest for the VAddr
                match addr_or_value {
                    ValueAddressOrValue::Value(v) => {
                        let new_v_addr = abstraction.new_vaddr(
                            proc_state,
                            *var_id,
                            &new_item.prog_loc_or_pid,
                            &new_item.env,
                            &new_item.time,
                        );
                        new_item.env.inner.insert(*var_id, new_v_addr.clone());

                        for state in push_to_value_store(
                            ast_helper,
                            seen_proc_states,
                            store,
                            new_v_addr,
                            v.clone(),
                        ) {
                            result.revisit.push((state, "abs_receive".to_string()));
                        }
                    }
                    ValueAddressOrValue::ValueAddress(v) => {
                        new_item.env.inner.insert(*var_id, v.clone());
                    }
                };
            }
        }
        result.new.push((new_item, "abs_receive".to_string()));
    }

    result
}
