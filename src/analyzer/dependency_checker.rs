use crate::{
    ast::{TypedCore, Var},
    state_space::{
        Kont, KontinuationAddress, Mailboxes, Pid, ProcState, ProgLocOrPid, Store, Value,
        ValueAddress, VarName,
    },
    util::{AstHelper, SetMap},
};

pub fn push_to_mailboxes<K: KontinuationAddress, V: ValueAddress>(
    ast_helper: &AstHelper,
    seen: &SetMap<Pid, ProcState<K, V>>,
    mailboxes: &mut Mailboxes<V>,
    pid: Pid,
    v_addr: V,
) -> Vec<ProcState<K, V>> {
    mailboxes.push(pid.clone(), v_addr);

    let mut dependencies = Vec::new();
    match seen.get(&pid) {
        Some(set) => {
            log::debug!(
                "push_to_mailboxes - Seen {:#?} with Pid {:#?}",
                set.len(),
                pid
            );
            for state in set {
                match state.prog_loc_or_pid {
                    ProgLocOrPid::ProgLoc(location) => {
                        match ast_helper.get(location) {
                            TypedCore::Receive(_) => {
                                // NOTE cloning here might become a memory issue
                                dependencies.push(state.clone());
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }
        _ => {}
    }
    dependencies
}

fn match_vaddr<K: KontinuationAddress, V: ValueAddress>(
    extended_v_addr: &V,
    state: &ProcState<K, V>,
    var: &Var,
    dependencies: &mut Vec<ProcState<K, V>>,
) {
    match state.env.inner.get(&VarName::from(var)) {
        Some(pl_vaddr) => {
            if pl_vaddr == extended_v_addr {
                // NOTE cloning here might become a memory issue
                dependencies.push(state.clone());
            }
        }
        _ => {}
    }
}

pub fn push_to_value_store<K: KontinuationAddress, V: ValueAddress>(
    ast_helper: &AstHelper,
    seen: &SetMap<Pid, ProcState<K, V>>,
    store: &mut Store<K, V>,
    v_addr: V,
    value: Value<V>,
) -> Vec<ProcState<K, V>> {
    store.value.push(v_addr.clone(), value);

    let mut dependencies = Vec::new();
    // for (_pid, states) in &seen.inner {
    //     log::debug!(
    //         "push_to_value_store - Seen {:#?} with Pid {:#?}",
    //         states.len(),
    //         _pid
    //     );
    //     for state in states {
    //         match state.prog_loc_or_pid {
    //             ProgLocOrPid::ProgLoc(location) => match ast_helper.get(location) {
    //                 TypedCore::Var(v) => match_vaddr(&v_addr, state, v, &mut dependencies),
    //                 TypedCore::Apply(apply) => {
    //                     for arg in &apply.args.inner {
    //                         match arg {
    //                             TypedCore::Var(v) => {
    //                                 match_vaddr(&v_addr, state, v, &mut dependencies)
    //                             }
    //                             _ => {}
    //                         }
    //                     }
    //                 }
    //                 TypedCore::Call(call) => {
    //                     for arg in &call.args.inner {
    //                         match arg {
    //                             TypedCore::Var(v) => {
    //                                 match_vaddr(&v_addr, state, v, &mut dependencies)
    //                             }
    //                             _ => {}
    //                         }
    //                     }
    //                 }
    //                 _ => {}
    //             },
    //             _ => {}
    //         }
    //     }
    // }
    dependencies
}

pub fn push_to_kont_store<K: KontinuationAddress, V: ValueAddress>(
    ast_helper: &AstHelper,
    seen: &SetMap<Pid, ProcState<K, V>>,
    store: &mut Store<K, V>,
    k_addr: K,
    kont: Kont<K, V>,
) -> Vec<ProcState<K, V>> {
    store.kont.push(k_addr.clone(), kont);

    let mut dependencies = Vec::new();
    // for (_pid, states) in &seen.inner {
    //     log::debug!(
    //         "push_to_kont_store - Seen {:#?} with Pid {:#?}",
    //         states.len(),
    //         _pid
    //     );
    //     for state in states {
    //         if state.k_addr != k_addr {
    //             continue;
    //         }
    //         match state.prog_loc_or_pid {
    //             ProgLocOrPid::ProgLoc(location) => match ast_helper.get(location) {
    //                 TypedCore::Module(_)
    //                 | TypedCore::Var(_)
    //                 | TypedCore::Apply(_)
    //                 | TypedCore::Call(_)
    //                 | TypedCore::LetRec(_)
    //                 | TypedCore::Case(_)
    //                 | TypedCore::Receive(_)
    //                 | TypedCore::PrimOp(_)
    //                 | TypedCore::Let(_) => {}
    //                 _ => dependencies.push(state.clone()),
    //             },
    //             ProgLocOrPid::Pid(_) => {
    //                 // NOTE cloning here might become a memory issue
    //                 dependencies.push(state.clone());
    //             }
    //         }
    //     }
    // }
    dependencies
}
