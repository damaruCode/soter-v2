use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::push_to_value_store,
    state_space::{
        Kont, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, Value, ValueAddress,
        VarName,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_pop_let_pid<K: KontinuationAddress, V: ValueAddress>(
    pid: &Pid,
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    let konts = store.kont.get(&proc_state.k_addr).unwrap().clone();
    for kont in konts {
        match kont {
            Kont::Let(var_list, body, env, k_addr) => {
                if var_list.len() != 1 {
                    panic!()
                }

                let var_name = VarName::from(ast_helper.get(var_list[0]));

                let mut new_item = proc_state.clone();
                new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc(body);
                new_item.env = env;
                new_item.k_addr = k_addr;

                let new_v_addr = abstraction.new_vaddr(
                    proc_state,
                    &var_name,
                    &new_item.prog_loc_or_pid,
                    &new_item.time,
                );

                new_item.env.inner.insert(var_name, new_v_addr.clone());

                for state in push_to_value_store(
                    ast_helper,
                    seen_proc_states,
                    store,
                    new_v_addr,
                    Value::Pid(pid.clone()),
                ) {
                    v_revisit.push((state, "abs_pop_let_pid".to_string()));
                }

                v_new.push((new_item, "abs_pop_let_pid".to_string()));
            }
            Kont::Stop => {} // successful halt
            Kont::Seq(_, _, _) => panic!(),
        }
    }

    log::debug!(
        "ABS_POP_LET_PID - {:?} New - {:?} Revisit",
        v_new.len(),
        v_revisit.len()
    );
    (v_new, v_revisit)
}
