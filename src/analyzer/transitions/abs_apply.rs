use crate::{
    abstraction::Abstraction,
    ast::{Apply, Index, TypedCore},
    state_space::{
        KontinuationAddress, ProcState, ProgLocOrPid, Store, Value, ValueAddress, VarName,
    },
    util::AstHelper,
};

use super::TransitionResult;

pub fn abs_apply<K: KontinuationAddress, V: ValueAddress>(
    apply: &Apply,
    proc_state: &ProcState<K, V>,
    store: &Store<K, V>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();

    match *apply.op.clone() {
        TypedCore::Var(v) => {
            let values = store
                .value
                .get(proc_state.env.inner.get(&VarName::from(&v)).unwrap())
                .unwrap();

            for value in values {
                match value {
                    Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                        TypedCore::Fun(f) => {
                            let apl_var_names = Vec::<VarName>::from(&apply.args);
                            let fn_var_names = Vec::<VarName>::from(&f.vars);

                            let mut new_env = clo.env.clone();
                            for i in 0..fn_var_names.len() {
                                new_env.inner.insert(
                                    fn_var_names[i].clone(),
                                    proc_state.env.inner.get(&apl_var_names[i]).unwrap().clone(),
                                );
                            }

                            let mut new_item = proc_state.clone();
                            new_item.prog_loc_or_pid =
                                ProgLocOrPid::ProgLoc((*f.body).get_index().unwrap());
                            new_item.env = new_env;
                            new_item.time = abstraction.tick(
                                &new_item.time,
                                match proc_state.prog_loc_or_pid {
                                    ProgLocOrPid::ProgLoc(pl) => pl,
                                    _ => panic!(),
                                },
                            );

                            v_new.push(new_item);
                        }
                        _ => panic!(),
                    },
                    _ => panic!(),
                }
            }
        }
        _ => panic!(),
    }

    (v_new, Vec::new())
}
