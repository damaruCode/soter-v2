use crate::{
    analyzer::dependency_checker::push_to_mailboxes,
    ast::{Index, TypedCore},
    state_space::r#abstract::{
        Closure, Env, KontinuationAddress, Mailboxes, Pid, ProcState, ProgLocOrPid, Store, Value,
        ValueAddress, VarName,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_send<K: KontinuationAddress, V: ValueAddress>(
    typed_core_to: &TypedCore,
    typed_core_msg: &TypedCore,
    proc_state: &ProcState<K, V>,
    mailboxes: &mut Mailboxes<V>,
    store: &Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    println!("{:#?}", typed_core_to);

    let pids = match typed_core_to {
        TypedCore::Var(v) => {
            let maybe_pids = store
                .value
                .get(proc_state.env.inner.get(&VarName::from(v)).unwrap())
                .unwrap();

            let mut pids = Vec::new();
            for maybe_pid in maybe_pids {
                match maybe_pid {
                    Value::Pid(pid) => pids.push(pid),
                    _ => panic!(),
                }
            }
            pids
        }
        _ => panic!(),
    };

    let msg_values = match typed_core_msg {
        TypedCore::Var(v) => store
            .value
            .get(proc_state.env.inner.get(&VarName::from(v)).unwrap())
            .unwrap(),
        TypedCore::Literal(l) => &Vec::from([Value::Closure(Closure {
            prog_loc: (*l.val).get_index().unwrap(),
            env: Env::init(),
        })]),
        _ => panic!(),
    };

    for pid in pids {
        for value in msg_values {
            let mut new_item = proc_state.clone();
            new_item.prog_loc_or_pid = match value {
                Value::Pid(pid) => ProgLocOrPid::Pid(pid.clone()),
                Value::Closure(clo) => ProgLocOrPid::ProgLoc(clo.prog_loc.clone()),
            };

            v_new.push(new_item);

            v_revisit.append(&mut push_to_mailboxes(
                ast_helper,
                seen_proc_states,
                mailboxes,
                pid.clone(),
                value.clone(),
            ));
        }
    }
    (v_new, v_revisit)
}
