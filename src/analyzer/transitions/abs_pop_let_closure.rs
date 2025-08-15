use crate::{
    analyzer::dependency_checker::push_to_value_store,
    state_space::r#abstract::{
        AddressBuilder, Closure, Env, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store,
        Value, ValueAddress, VarName,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_pop_let_closure<K: KontinuationAddress, V: ValueAddress>(
    proc_state: &ProcState<K, V>,
    proc_state_prog_loc: usize,
    kont_var_list: Vec<usize>,
    kont_body_prog_loc: usize,
    kont_env: &Env<V>,
    kont_k_addr: &K,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    address_builder: &Box<dyn AddressBuilder<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    if kont_var_list.len() != 1 {
        panic!("Expected VarList of length 1");
    }

    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    let mut new_item = proc_state.clone();
    new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(kont_body_prog_loc);
    new_item.env = kont_env.clone();
    new_item.k_addr = kont_k_addr.clone();
    let new_var_name = VarName::from(ast_helper.get(kont_var_list[0]));
    let new_v_addr = address_builder.new_vaddr(
        &proc_state,
        &new_var_name,
        &new_item.prog_loc_or_pid,
        &new_item.time,
    );
    new_item
        .env
        .inner
        .insert(new_var_name.clone(), new_v_addr.clone());

    v_revisit.append(&mut push_to_value_store(
        &ast_helper,
        seen_proc_states,
        store,
        new_v_addr,
        Value::Closure(Closure {
            prog_loc: proc_state_prog_loc,
            env: proc_state.env.clone(),
        }),
    ));
    v_new.push(new_item);

    (v_new, v_revisit)
}
