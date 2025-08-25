use std::collections::VecDeque;

use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::push_to_kont_store,
    ast::TypedCore,
    state_space::{
        Closure, Env, Kont, KontinuationAddress, Pid, ProcState, ProgLoc, ProgLocOrPid, Store,
        Value, ValueAddress,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_pop_apply<K: KontinuationAddress, V: ValueAddress>(
    proc_state: &ProcState<K, V>,
    kont_arg_list: &VecDeque<ProgLoc>,
    kont_value_list: &Vec<Value<V>>,
    kont_module_atom: Option<TypedCore>,
    kont_op_value: &Value<V>,
    kont_env: &Env<V>,
    kont_k_addr: &K,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    let mut new_item = proc_state.clone();
    let mut new_kont_arg_list = kont_arg_list.clone();
    new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(new_kont_arg_list.pop_front().unwrap());
    new_item.env = kont_env.clone();
    new_item.k_addr = abstraction.new_kaddr(
        proc_state,
        &new_item.prog_loc_or_pid,
        &new_item.env,
        &new_item.time,
    );

    let value = match &proc_state.prog_loc_or_pid {
        ProgLocOrPid::Pid(pid) => Value::Pid(pid.clone()),
        ProgLocOrPid::ProgLoc(prog_loc) => Value::Closure(Closure {
            prog_loc: *prog_loc,
            env: Env::init(),
        }),
    };

    let mut new_value_list = kont_value_list.clone();
    new_value_list.push(value);

    let kont = Kont::Apply(
        new_kont_arg_list.clone(),
        new_value_list,
        kont_module_atom.clone(),
        kont_op_value.clone(),
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
        v_revisit.push((state, "abs_pop_apply".to_string()));
    }

    v_new.push((new_item, "abs_pop_apply".to_string()));

    log::debug!(
        "ABS_POP_APPLY - {:?} New - {:?} Revisit",
        v_new.len(),
        v_revisit.len()
    );
    (v_new, v_revisit)
}
