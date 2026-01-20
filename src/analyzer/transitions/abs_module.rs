use crate::{
    abstraction::Abstraction,
    ast::{Index, Module, TypedCore},
    state_space::{
        Closure, Env, FailureType, KontinuationAddress, ProcState, ProgLocOrPid, Store, Value,
        ValueAddress, VarName,
    },
    util::AstHelper,
};

use super::TransitionResult;

pub fn abs_module<K: KontinuationAddress, V: ValueAddress>(
    module: &Module,
    proc_state: &ProcState<K, V>,
    store: &mut Store<K, V>,
    module_env: &mut Env<V>,
    abstraction: &Box<dyn Abstraction<K, V>>,
    ast_helper: &AstHelper,
) -> TransitionResult<K, V> {
    let mut result = TransitionResult::new();

    let mut new_item = proc_state.clone();
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
                    &new_item.env,
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
                    _ => {
                        result.new.push((
                            proc_state.fail(FailureType::General),
                            "abs_module".to_string(),
                        ));
                        continue;
                    }
                };
            }
            _ => {
                result.new.push((
                    proc_state.fail(FailureType::General),
                    "abs_module".to_string(),
                ));
                continue;
            }
        }
    }

    match new_item
        .env
        .inner
        .get(&VarName::FnAtom("main".to_string(), 0))
    {
        Some(v) => match store.value.get(v) {
            Some(values) => {
                for value in values {
                    match value {
                        Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                            TypedCore::Fun(f) => match &*f.body {
                                TypedCore::Case(c) => match &c.clauses.inner[0] {
                                    TypedCore::Clause(c) => {
                                        new_item.prog_loc_or_pid =
                                            ProgLocOrPid::ProgLoc((*c.body).get_index().unwrap());
                                    }
                                    _ => {
                                        result.new.push((
                                            proc_state.fail(FailureType::General),
                                            "abs_module".to_string(),
                                        ));
                                    }
                                },
                                _ => {
                                    result.new.push((
                                        proc_state.fail(FailureType::General),
                                        "abs_module".to_string(),
                                    ));
                                }
                            },
                            _ => {
                                result.new.push((
                                    proc_state.fail(FailureType::General),
                                    "abs_module".to_string(),
                                ));
                            }
                        },
                        _ => {
                            result.new.push((
                                proc_state.fail(FailureType::General),
                                "abs_module".to_string(),
                            ));
                        }
                    }
                }
            }
            _ => {
                result.new.push((
                    proc_state.fail(FailureType::General),
                    "abs_module".to_string(),
                ));
            }
        },
        _ => {
            result.new.push((
                proc_state.fail(FailureType::General),
                "abs_module".to_string(),
            ));
        }
    }

    // ... also update the module_env
    module_env.merge_with(&new_item.env);

    result.new.push((new_item, "abs_module".to_string()));

    result
}
