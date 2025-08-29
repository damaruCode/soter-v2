use crate::{
    abstraction::Abstraction,
    analyzer::{dependency_checker::push_to_kont_store, transitions::abs_self},
    ast::{Index, TypedCore},
    state_space::{
        Kont, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, ValueAddress, VarName,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_push_call<K: KontinuationAddress, V: ValueAddress>(
    module: &TypedCore,
    op: &TypedCore,
    args: &Vec<TypedCore>,
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    let mut new_item = proc_state.clone();
    new_item.k_addr = abstraction.new_kaddr(
        &proc_state,
        &new_item.prog_loc_or_pid,
        &new_item.env,
        &new_item.time,
    );

    let op_name = VarName::from(op);

    let module_name = VarName::from(module);

    if module_name == VarName::Atom("erlang".to_string()) {
        let kont;
        if op_name == VarName::Atom("send".to_string()) || op_name == VarName::Atom("!".to_string())
        {
            new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(args[0].get_index().unwrap());
            // Send
            kont = Kont::Send(
                Some(args[1].get_index().unwrap()), // message prog_loc
                None,
                proc_state.env.clone(),
                proc_state.k_addr.clone(),
            );
            for state in push_to_kont_store(
                &ast_helper,
                &seen_proc_states,
                store,
                new_item.k_addr.clone(),
                kont,
            ) {
                v_revisit.push((state, "abs_push_call".to_string()));
            }
        } else if op_name == VarName::Atom("spawn".to_string()) {
            new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(args[0].get_index().unwrap());
            // Spawn
            kont = Kont::Spawn(proc_state.k_addr.clone());

            for state in push_to_kont_store(
                &ast_helper,
                &seen_proc_states,
                store,
                new_item.k_addr.clone(),
                kont,
            ) {
                v_revisit.push((state, "abs_push_call".to_string()));
            }
        } else if op_name == VarName::Atom("self".to_string()) {
            return abs_self(proc_state);
        } else {
            panic!()
        }
    } else {
        panic!()
    }
    v_new.push((new_item, "abs_push_call".to_string()));

    log::debug!(
        "ABS_PUSH_CALL - {:?} New - {:?} Revisit",
        v_new.len(),
        v_revisit.len()
    );

    (v_new, v_revisit)
}
