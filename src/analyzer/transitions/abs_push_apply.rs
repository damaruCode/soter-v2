use std::collections::VecDeque;

use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::push_to_kont_store,
    ast::{Index, TypedCore},
    state_space::{
        Kont, KontinuationAddress, Pid, ProcState, ProgLoc, ProgLocOrPid, Store, ValueAddress,
        VarName,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

fn push_apply<K: KontinuationAddress, V: ValueAddress>(
    args: &Vec<TypedCore>,
    proc_state: &ProcState<K, V>,
    new_item: ProcState<K, V>,
    op_name: &VarName,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    for op_value in &store
        .value
        .get(proc_state.env.inner.get(&op_name).unwrap())
        .unwrap()
        .clone()
    {
        let mut arg_list: VecDeque<usize> = (&args
            .iter()
            .map(|tc| tc.get_index().unwrap())
            .collect::<Vec<ProgLoc>>())
            .clone()
            .into();
        arg_list.pop_front();

        let kont = Kont::Apply(
            arg_list,
            Vec::new(),
            op_value.clone(),
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
            v_revisit.push((state, "abs_push_apply".to_string()));
        }
    }
    v_new.push((new_item, "abs_push_apply".to_string()));

    (v_new, v_revisit)
}

pub fn abs_push_apply<K: KontinuationAddress, V: ValueAddress>(
    op: &TypedCore,
    args: &Vec<TypedCore>,
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let v_new;
    let v_revisit;

    let mut new_item = proc_state.clone();
    new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(args[0].get_index().unwrap());
    new_item.k_addr = abstraction.new_kaddr(
        &proc_state,
        &new_item.prog_loc_or_pid,
        &new_item.env,
        &new_item.time,
    );

    let op_name = VarName::from(op);

    (v_new, v_revisit) = push_apply(
        args,
        proc_state,
        new_item,
        &op_name,
        store,
        seen_proc_states,
        ast_helper,
    );

    log::debug!(
        "ABS_PUSH_APPLY - {:?} New - {:?} Revisit",
        v_new.len(),
        v_revisit.len()
    );

    (v_new, v_revisit)
}
