use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::push_to_value_store,
    state_space::{
        Closure, Env, FailureType, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, Value,
        ValueAddress, VarName,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_pop_let_closure<K: KontinuationAddress, V: ValueAddress>(
    proc_state: &ProcState<K, V>,
    proc_state_prog_loc: usize,
    kont_var_list: &Vec<usize>,
    kont_body_prog_loc: usize,
    kont_env: &Env<V>,
    kont_k_addr: &K,
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
            "abs_pop_let_closure".to_string(),
        ));
        return result;
    }

    let mut result = TransitionResult::new();

    let mut new_item = proc_state.clone();
    new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(kont_body_prog_loc);
    new_item.env = kont_env.clone();
    new_item.k_addr = kont_k_addr.clone();
    let new_var_name = VarName::from(ast_helper.get(kont_var_list[0]));

    match ast_helper.get_var(&new_var_name) {
        Some(var_id) => {
            let new_v_addr = abstraction.new_vaddr(
                &proc_state,
                var_id,
                &new_item.prog_loc_or_pid,
                &new_item.env,
                &new_item.time,
            );
            new_item
                .env
                .inner
                .insert(new_var_name.clone(), new_v_addr.clone());

            for state in push_to_value_store(
                &ast_helper,
                seen_proc_states,
                store,
                new_v_addr,
                Value::Closure(Closure {
                    prog_loc: proc_state_prog_loc,
                    env: proc_state.env.clone(),
                }),
            ) {
                result
                    .revisit
                    .push((state, "abs_pop_let_closure".to_string()));
            }
            result
                .new
                .push((new_item, "abs_pop_let_closure".to_string()));
        }
        None => {
            result.new.push((
                proc_state.fail(FailureType::Unexpected(format!(
                    "Could not find \"{}\" in AST.",
                    new_var_name
                ))),
                "abs_pop_let_closure".to_string(),
            ));
        }
    }

    result
}
