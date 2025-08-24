use crate::{
    ast::TypedCore,
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
    value: Value<V>,
) -> Vec<ProcState<K, V>> {
    mailboxes.push(pid.clone(), value);

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

pub fn push_to_value_store<K: KontinuationAddress, V: ValueAddress>(
    ast_helper: &AstHelper,
    seen: &SetMap<Pid, ProcState<K, V>>,
    store: &mut Store<K, V>,
    v_addr: V,
    value: Value<V>,
) -> Vec<ProcState<K, V>> {
    store.value.push(v_addr.clone(), value);

    let mut dependencies = Vec::new();
    for (_pid, states) in &seen.inner {
        log::debug!(
            "push_to_value_store - Seen {:#?} with Pid {:#?}",
            states.len(),
            _pid
        );
        for state in states {
            match state.prog_loc_or_pid {
                ProgLocOrPid::ProgLoc(location) => match ast_helper.get(location) {
                    TypedCore::Var(pl_var) => {
                        match state.env.inner.get(&VarName::from(&*pl_var.name)) {
                            Some(pl_vaddr) => {
                                if pl_vaddr == &v_addr {
                                    // NOTE cloning here might become a memory issue
                                    dependencies.push(state.clone());
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                },
                _ => {}
            }
        }
    }
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
    for (_pid, states) in &seen.inner {
        log::debug!(
            "push_to_kont_store - Seen {:#?} with Pid {:#?}",
            states.len(),
            _pid
        );
        for state in states {
            if state.k_addr != k_addr {
                continue;
            }
            match state.prog_loc_or_pid {
                ProgLocOrPid::ProgLoc(location) => match ast_helper.get(location) {
                    TypedCore::Module(_)
                    | TypedCore::Var(_)
                    | TypedCore::Apply(_)
                    | TypedCore::Call(_)
                    | TypedCore::LetRec(_)
                    | TypedCore::Case(_)
                    | TypedCore::Receive(_)
                    | TypedCore::PrimOp(_)
                    | TypedCore::Let(_) => {}
                    _ => dependencies.push(state.clone()),
                },
                ProgLocOrPid::Pid(_) => {
                    // NOTE cloning here might become a memory issue
                    dependencies.push(state.clone());
                }
            }
        }
    }
    dependencies
}
