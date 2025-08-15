use crate::{
    ast::TypedCore,
    state_space::r#abstract::{
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
    for (_, states) in &seen.inner {
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
    for (_, states) in &seen.inner {
        for state in states {
            if state.k_addr != k_addr {
                continue;
            }
            match state.prog_loc_or_pid {
                ProgLocOrPid::ProgLoc(location) => match ast_helper.get(location) {
                    // TODO add the arms that are non-reducable
                    _ => {}
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
