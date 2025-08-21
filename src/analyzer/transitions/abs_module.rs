use crate::{
    abstraction::Abstraction,
    ast::{Index, Module, TypedCore},
    state_space::{
        Closure, Env, KontinuationAddress, ProcState, ProgLocOrPid, Store, Value, ValueAddress,
        VarName,
    },
};

use super::TransitionResult;

pub fn abs_module<K: KontinuationAddress, V: ValueAddress>(
    module: &Module,
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    module_env: &mut Env<V>,
    abstraction: &Box<dyn Abstraction<K, V>>,
) -> TransitionResult<K, V> {
    let mut v_new = Vec::new();

    // We'll skip into the main function
    let mut new_item = proc_state.clone();
    for def in &*module.defs.inner {
        match &*def.frst {
            TypedCore::Var(v) => match VarName::from(v) {
                VarName::FnAtom(atom, arity) => match atom.as_str() == "main" && arity == 0 {
                    true => match &*def.scnd {
                        TypedCore::Fun(f) => match &*f.body {
                            TypedCore::Case(c) => match &c.clauses.inner[0] {
                                TypedCore::Clause(c) => {
                                    new_item.prog_loc_or_pid =
                                        ProgLocOrPid::ProgLoc((*c.body).get_index().unwrap());

                                    break;
                                }
                                _ => panic!(),
                            },
                            _ => panic!(),
                        },
                        _ => panic!(),
                    },
                    _ => todo!(),
                },
                _ => todo!(),
            },
            _ => panic!(),
        };
    }

    // For every definition in the module...
    for def in &module.defs.inner {
        match &*def.frst {
            TypedCore::Var(v) => {
                // ... generate a v_addr for the right-hand function (using the left-hand var_name)...
                let var_name = VarName::from(v);
                let new_v_addr = abstraction.new_vaddr(
                    proc_state,
                    &var_name,
                    &new_item.prog_loc_or_pid,
                    &new_item.time,
                );

                // ... and insert it into the local environment of the next proc_state...
                new_item
                    .env
                    .inner
                    .insert(var_name.clone(), new_v_addr.clone());

                match &*def.scnd {
                    TypedCore::Fun(_) => {
                        // ... as well as into the store
                        store.value.push(
                            // NOTE we skip push_to_value_store, because there are no
                            // dependencies to check
                            new_v_addr,
                            Value::Closure(Closure {
                                prog_loc: (*def.scnd).get_index().unwrap(),
                                env: Env::init(),
                            }),
                        );
                    }
                    _ => panic!(),
                };
            }
            _ => panic!(),
        }
    }

    // ... also update the module_env
    module_env.merge_with(&new_item.env);

    v_new.push((new_item, "abs_module".to_string()));

    log::debug!("ABS_PUSH_MODULE - {:?} New - {:?} Revisit", v_new.len(), 0);
    (v_new, Vec::new())
}
