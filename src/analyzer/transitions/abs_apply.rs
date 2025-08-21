use crate::{
    abstraction::Abstraction,
    analyzer::dependency_checker::push_to_value_store,
    ast::{Apply, Index, TypedCore},
    state_space::{
        Closure, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Store, Value, ValueAddress,
        VarName,
    },
    util::{AstHelper, SetMap},
};

use super::TransitionResult;

pub fn abs_apply<K: KontinuationAddress, V: ValueAddress>(
    apply: &Apply,
    prog_loc_proc_state: usize,
    proc_state: &ProcState<K, V>,
    seen_proc_states: &SetMap<Pid, ProcState<K, V>>,
    store: &mut Store<K, V>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();
    let mut v_revisit = Vec::new();

    match *apply.op.clone() {
        TypedCore::Var(v) => {
            let values = store
                .value
                .get(proc_state.env.inner.get(&VarName::from(&v)).unwrap())
                .unwrap()
                .clone();

            for value in values {
                match value {
                    Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                        TypedCore::Fun(f) => {
                            let fn_var_names = Vec::<VarName>::from(&f.vars);

                            let mut new_item = proc_state.clone();
                            new_item.prog_loc_or_pid =
                                ProgLocOrPid::ProgLoc((*f.body).get_index().unwrap());
                            new_item.time = abstraction.tick(&new_item.time, prog_loc_proc_state);

                            let mut new_env = clo.env.clone();
                            for i in 0..fn_var_names.len() {
                                // check the type of the arg
                                match &apply.args.inner[i] {
                                    // for vars, we simply add a binding to the existent v_addr
                                    TypedCore::Var(v) => {
                                        new_env.inner.insert(
                                            fn_var_names[i].clone(),
                                            proc_state
                                                .env
                                                .inner
                                                .get(&VarName::from(v))
                                                .unwrap()
                                                .clone(),
                                        );
                                    }
                                    // for literals, we add a new v_addr according to the
                                    // fn_var_name
                                    TypedCore::Literal(_) => {
                                        let v_addr = abstraction.new_vaddr(
                                            proc_state,
                                            &fn_var_names[i],
                                            &new_item.prog_loc_or_pid,
                                            &new_item.env,
                                            &new_item.time,
                                        );

                                        new_env
                                            .inner
                                            .insert(fn_var_names[i].clone(), v_addr.clone());

                                        for state in push_to_value_store(
                                            ast_helper,
                                            seen_proc_states,
                                            store,
                                            v_addr,
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
                            new_item.env = new_env;

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
