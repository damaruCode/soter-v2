use crate::{
    analyzer::dependency_checker::push_to_mailboxes,
    ast::{Index, TypedCore},
    state_space::{
        Closure, KontinuationAddress, Mailboxes, Pid, ProcState, ProgLoc, ProgLocOrPid, Store,
        Value, ValueAddress, VarName,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_pop_send<K: KontinuationAddress, V: ValueAddress>(
    kont_msg_prog_loc: ProgLoc,
    kont_k_addr: &K,
    proc_state: &ProcState<K, V>,
    mailboxes: &mut Mailboxes<V>,
    store: &Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    // proc_state.prog_loc_or_pid should be a pid now
    let mut pids = Vec::new();

    match &proc_state.prog_loc_or_pid {
        ProgLocOrPid::Pid(pid) => pids.push(pid.clone()),
        ProgLocOrPid::ProgLoc(prog_loc) => match ast_helper.get(*prog_loc) {
            TypedCore::Var(v) => {
                let maybe_pids = store
                    .value
                    .get(proc_state.env.inner.get(&VarName::from(v)).unwrap())
                    .unwrap();

                for value in maybe_pids {
                    match value {
                        Value::Pid(pid) => pids.push(pid.clone()),
                        _ => {} // ignore
                    }
                }
            }
            _ => {} //ignore
        },
    };

    for pid in pids {
        let mut new_item = proc_state.clone();
        new_item.prog_loc_or_pid = ProgLocOrPid::Pid(pid.clone());
        new_item.k_addr = kont_k_addr.clone();

        // kont_msg_prog_loc should be value
        let typed_core_msg = ast_helper.get(kont_msg_prog_loc);
        let msg_values = match typed_core_msg {
            TypedCore::Literal(_)
            | TypedCore::AstList(_)
            | TypedCore::Tuple(_)
            | TypedCore::Fun(_) => Vec::from([Value::Closure(Closure {
                prog_loc: typed_core_msg.get_index().unwrap(),
                env: proc_state.env.clone(),
            })]),
            _ => panic!(),
        };

        for value in msg_values {
            for state in push_to_mailboxes(
                ast_helper,
                seen_proc_states,
                mailboxes,
                pid.clone(),
                value.clone(),
            ) {
                v_revisit.push((state, "abs_send".to_string()));
            }
            v_new.push((new_item.clone(), "abs_send".to_string()));
        }
    }

    log::debug!(
        "ABS_SEND - {:?} New - {:?} Revisit",
        v_new.len(),
        v_revisit.len()
    );
    (v_new, v_revisit)
}
