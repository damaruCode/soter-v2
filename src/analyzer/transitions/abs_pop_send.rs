use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::{push_to_kont_store, push_to_mailboxes},
    ast::{Index, TypedCore},
    state_space::{
        Closure, Env, Kont, KontinuationAddress, Mailboxes, Pid, ProcState, ProgLoc, ProgLocOrPid,
        Store, Value, ValueAddress,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_pop_send<K: KontinuationAddress, V: ValueAddress>(
    kont_msg_prog_loc: Option<ProgLoc>,
    kont_pid_value: Option<Pid>,
    kont_env: &Env<V>,
    kont_k_addr: &K,
    proc_state: &ProcState<K, V>,
    mailboxes: &mut Mailboxes<V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    match kont_pid_value {
        Some(pid) => {
            // ABS_SEND
            let mut new_item = proc_state.clone();
            new_item.prog_loc_or_pid = ProgLocOrPid::Pid(pid.clone());
            new_item.k_addr = kont_k_addr.clone();

            let msg_value = match &proc_state.prog_loc_or_pid {
                ProgLocOrPid::ProgLoc(prog_loc) => {
                    let typed_core_msg = ast_helper.get(*prog_loc);

                    match typed_core_msg {
                        TypedCore::Literal(_)
                        | TypedCore::AstList(_)
                        | TypedCore::Tuple(_)
                        | TypedCore::Fun(_) => Value::Closure(Closure {
                            prog_loc: typed_core_msg.get_index().unwrap(),
                            env: proc_state.env.clone(),
                        }),
                        _ => panic!(),
                    }
                }
                ProgLocOrPid::Pid(pid) => Value::Pid(pid.clone()),
            };

            for state in push_to_mailboxes(
                ast_helper,
                seen_proc_states,
                mailboxes,
                pid.clone(),
                msg_value.clone(),
            ) {
                v_revisit.push((state, "abs_send".to_string()));
            }
            v_new.push((new_item, "abs_send".to_string()));
        }
        None => {
            // ABS_POP_SEND
            let mut new_item = proc_state.clone();
            new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(match kont_msg_prog_loc {
                Some(prog_loc) => prog_loc,
                _ => panic!(),
            });
            new_item.env = kont_env.clone();
            new_item.k_addr = abstraction.new_kaddr(
                proc_state,
                &new_item.prog_loc_or_pid,
                &new_item.env,
                &new_item.time,
            );

            let kont = Kont::Send(
                None,
                match &proc_state.prog_loc_or_pid {
                    ProgLocOrPid::Pid(pid) => Some(pid.clone()),
                    _ => panic!(),
                },
                kont_env.clone(),
                kont_k_addr.clone(),
            );

            for state in push_to_kont_store(
                ast_helper,
                seen_proc_states,
                store,
                new_item.k_addr.clone(),
                kont,
            ) {
                v_revisit.push((state, "abs_pop_send".to_string()));
            }
            v_new.push((new_item, "abs_pop_send".to_string()));
        }
    }

    log::debug!(
        "ABS_SEND / ABS_POP_SEND - {:?} New - {:?} Revisit",
        v_new.len(),
        v_revisit.len()
    );
    (v_new, v_revisit)
}
