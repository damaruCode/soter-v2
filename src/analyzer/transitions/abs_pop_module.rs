use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::{push_to_kont_store, push_to_value_store},
    ast::{Index, TypedCore},
    state_space::{
        Closure, Env, Kont, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, Value,
        ValueAddress, VarName,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

// TODO formalize and revisit --- I may have made some mistakes
pub fn abs_pop_module_fun<K: KontinuationAddress, V: ValueAddress>(
    kont_index: usize,
    kont_env: &Env<V>,
    kont_k_addr: &K, // NOTE right now the k_addr will naturally point to the Stop continuation
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    // NOTE by convention module has index 0
    match ast_helper.get(0) {
        TypedCore::Module(module) => {
            if module.defs.inner.len() <= kont_index + 1 {
                return (Vec::new(), Vec::new()); // skip
            }

            let mut v_new = Vec::new();
            let mut v_revisit = Vec::new();

            let mut new_item = proc_state.clone();
            new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(
                (*module.defs.inner[kont_index + 1].scnd)
                    .get_index()
                    .unwrap(),
            ); // straight to the fun
            new_item.env = kont_env.clone();

            let var_name = VarName::from(&*module.defs.inner[kont_index].frst);
            let new_v_addr = abstraction.new_vaddr(
                proc_state,
                &var_name,
                &new_item.prog_loc_or_pid,
                &new_item.time,
            );

            new_item.env.inner.insert(var_name, new_v_addr.clone());
            v_revisit.append(&mut push_to_value_store(
                ast_helper,
                seen_proc_states,
                store,
                new_v_addr,
                Value::Closure(Closure {
                    prog_loc: module.defs.inner[kont_index].scnd.get_index().unwrap(),
                    env: proc_state.env.clone(),
                }),
            ));

            let kont = Kont::Module(kont_index + 1, proc_state.env.clone(), kont_k_addr.clone());
            let new_k_addr = abstraction.new_kaddr(
                proc_state,
                &new_item.prog_loc_or_pid,
                &new_item.env,
                &new_item.time,
            );

            new_item.k_addr = new_k_addr.clone();
            v_revisit.append(&mut push_to_kont_store(
                ast_helper,
                seen_proc_states,
                store,
                new_k_addr,
                kont,
            ));

            v_new.push(new_item);

            log::debug!(
                "ABS_POP_MODULE - {:?} New - {:?} Revisit",
                v_new.len(),
                v_revisit.len()
            );

            (v_new, v_revisit)
        }
        _ => panic!(),
    }
}
