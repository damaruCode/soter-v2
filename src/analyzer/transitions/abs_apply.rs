use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::push_to_value_store,
    ast::{Index, TypedCore},
    state_space::{
        Closure, Env, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, Value,
        ValueAddress, VarName,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_apply<K: KontinuationAddress, V: ValueAddress>(
    kont_op_value: &Value<V>,
    kont_value_list: &Vec<Value<V>>,
    kont_env: &Env<V>,
    kont_k_addr: &K,
    proc_state: &ProcState<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    store: &mut Store<K, V>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    let mut new_item = proc_state.clone();
    new_item.env = kont_env.clone();
    new_item.k_addr = kont_k_addr.clone();

    // expand kont_value_list with the value in the control string position
    let mut value_list = kont_value_list.clone();
    value_list.push(match &proc_state.prog_loc_or_pid {
        ProgLocOrPid::Pid(pid) => Value::Pid(pid.clone()),
        ProgLocOrPid::ProgLoc(prog_loc) => Value::Closure(Closure {
            prog_loc: *prog_loc,
            env: kont_env.clone(),
        }),
    });

    match kont_op_value {
        Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
            TypedCore::Fun(f) => {
                let fn_var_names = Vec::<VarName>::from(&f.vars);

                new_item.prog_loc_or_pid = ProgLocOrPid::ProgLoc((*f.body).get_index().unwrap());
                new_item.time = abstraction.tick(
                    &new_item.time,
                    match proc_state.prog_loc_or_pid {
                        ProgLocOrPid::ProgLoc(pl) => pl,
                        _ => panic!(),
                    },
                );

                // the number of supplied values should match the number of function arguments
                if value_list.len() != fn_var_names.len() {
                    panic!()
                }

                for i in 0..fn_var_names.len() {
                    let new_v_addr = abstraction.new_vaddr(
                        proc_state,
                        &fn_var_names[i],
                        &new_item.prog_loc_or_pid,
                        &new_item.env,
                        &new_item.time,
                    );
                    new_item
                        .env
                        .inner
                        .insert(fn_var_names[i].clone(), new_v_addr.clone());

                    for state in push_to_value_store(
                        ast_helper,
                        seen_proc_states,
                        store,
                        new_v_addr,
                        value_list[i].clone(),
                    ) {
                        v_revisit.push((state, "abs_apply".to_string()));
                    }
                }
                v_new.push((new_item.clone(), "abs_apply".to_string()));
            }
            _ => panic!(),
        },
        _ => panic!(),
    }

    log::debug!(
        "ABS_APPLY - {:?} New - {:?} Revisit",
        v_new.len(),
        v_revisit.len()
    );
    (v_new, v_revisit)
}
