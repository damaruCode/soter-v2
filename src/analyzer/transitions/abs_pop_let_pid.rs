use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::push_to_value_store,
    ast::TypedCore,
    state_space::{
        Env, FailureType, KontinuationAddress, Pid, ProcState, ProgLoc, ProgLocOrPid, Store, Value,
        ValueAddress,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_pop_let_pid<K: KontinuationAddress, V: ValueAddress>(
    pid: &Pid,
    kont_var_list: &Vec<ProgLoc>,
    kont_body: ProgLoc,
    kont_env: &Env<V>,
    kont_k_addr: &K,
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();

    if kont_var_list.len() != 1 {
        result.new.push((
            proc_state.fail(FailureType::Unexpected(format!(
                "Expected contiuation var list of length 1, found length {}",
                kont_var_list.len()
            ))),
            "abs_pop_let_pid".to_string(),
        ));
        return result;
    }

    let mut new_item = proc_state.clone();
    new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(kont_body);
    new_item.env = kont_env.clone();
    new_item.k_addr = kont_k_addr.clone();

    match ast_helper.get(kont_var_list[0]) {
        TypedCore::Var(v) => {
            let var_id = v.var_id.unwrap();

            let new_v_addr = abstraction.new_vaddr(
                proc_state,
                *var_id,
                &new_item.prog_loc_or_pid,
                &new_item.env,
                &new_item.time,
            );

            new_item.env.inner.insert(*var_id, new_v_addr.clone());

            for state in push_to_value_store(
                ast_helper,
                seen_proc_states,
                store,
                new_v_addr,
                Value::Pid(pid.clone()),
            ) {
                result.revisit.push((state, "abs_pop_let_pid".to_string()));
            }

            result.new.push((new_item, "abs_pop_let_pid".to_string()));
        }
        tc => result.new.push((
            proc_state.fail(FailureType::Unexpected(format!(
                "Expected a variable, found {}",
                tc
            ))),
            "abs_pop_let_pid".to_string(),
        )),
    }

    result
}
