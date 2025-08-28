use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::push_to_value_store,
    ast::{Apply, Index, TypedCore},
    state_space::{
        Closure, Env, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, Value,
        ValueAddress, VarName,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_apply<K: KontinuationAddress, V: ValueAddress>(
    apply: &Apply,
    prog_loc_proc_state: usize,
    proc_state: &ProcState<K, V>,
    module_env: &Env<V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    store: &mut Store<K, V>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    match &*apply.op.clone() {
        TypedCore::Var(v) => {
            let op_values = store
                .value
                .get(proc_state.env.inner.get(&VarName::from(v)).unwrap())
                .unwrap()
                .clone();

            for op_value in &op_values {
                match op_value {
                    Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                        TypedCore::Fun(f) => {
                            let fn_var_names = Vec::<VarName>::from(&f.vars);

                            let mut new_item = proc_state.clone();
                            new_item.prog_loc_or_pid =
                                ProgLocOrPid::ProgLoc((*f.body).get_index().unwrap());
                            new_item.time = abstraction.tick(&new_item.time, prog_loc_proc_state);

                            new_item.env = clo.env.clone();
                            new_item.env.merge_with(module_env);
                            for i in 0..fn_var_names.len() {
                                // check the type of the arg
                                match &apply.args.inner[i] {
                                    // for vars, we simply add a binding to the existent v_addr
                                    TypedCore::Var(v) => {
                                        new_item.env.inner.insert(
                                            fn_var_names[i].clone(),
                                            proc_state
                                                .env
                                                .inner
                                                .get(&VarName::from(v))
                                                .unwrap()
                                                .clone(),
                                        );
                                    }
                                    TypedCore::Literal(_) // NOTE could handle this with a constant
                                                          // address
                                    | TypedCore::AstList(_)
                                    | TypedCore::Tuple(_)
                                    | TypedCore::Fun(_) => {
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
                                            Value::Closure(Closure {
                                                prog_loc: apply.args.inner[i].get_index().unwrap(),
                                                env: proc_state.env.clone(),
                                            }),
                                        ) {
                                            v_revisit.push((state, "abs_apply".to_string()));
                                        }
                                    }
                                    _ => panic!(),
                                }
                            }

                            v_new.push((new_item, "abs_apply".to_string()));
                        }
                        _ => panic!(),
                    },
                    _ => panic!(),
                }
            }
        }
        _ => panic!(),
    }

    log::debug!("ABS_APPLY - {:?} New - {:?} Revisit", v_new.len(), 0);
    (v_new, v_revisit)
}
