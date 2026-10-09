use crate::{
    ast::TypedCore,
    state_space::{
        Kont, KontinuationAddress, Mailboxes, Pid, ProcState, ProgLocOrPid, Store, Value,
        ValueAddress,
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
    if let Some(set) = seen.get(&pid) {
        for state in set {
            if let ProgLocOrPid::ProgLoc(location) = state.prog_loc_or_pid {
                if let TypedCore::Receive(_) = ast_helper.get(location) {
                    // NOTE cloning here might become a memory issue
                    dependencies.push(state.clone());
                }
            }
        }
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
    if !store.value.push(v_addr.clone(), value) {
        // if this value is already part of the store, ignore it
        return Vec::new();
    }

    let mut dependencies = Vec::new();
    for (_pid, states) in &seen.inner {
        for state in states {
            if let ProgLocOrPid::ProgLoc(location) = state.prog_loc_or_pid { if let TypedCore::Var(pl_var) = ast_helper.get(location) {
                let var_id = pl_var.var_id.unwrap();
                if let Some(pl_vaddr) = state.env.inner.get(var_id) {
                    if pl_vaddr == &v_addr {
                        // NOTE cloning here might become a memory issue
                        dependencies.push(state.clone());
                    }
                }
            } }
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
    if !store.kont.push(k_addr.clone(), kont) {
        // if this value is already part of the store, ignore it
        return Vec::new();
    }

    let mut dependencies = Vec::new();
    for (_pid, states) in &seen.inner {
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
