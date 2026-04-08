use crate::{
    abstraction::Abstraction,
    ast::{Index, MaybeIndex, Module, TypedCore},
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

    let mut main_var_id = MaybeIndex::None;

    // For every definition in the module...
    for def in &module.defs.inner {
        match &*def.frst {
            TypedCore::Var(v) => {
                let var_id = v.var_id.unwrap();
                // ... check if it is main/0 for later reference
                if &VarName::try_from(&*v.name).unwrap() == &VarName::FnAtom("main".to_string(), 0)
                {
                    // NOTE the compiler already rules out multiple declarations of main/0
                    // so the if block is a kind of sanity check, that should never
                    // actually execute (by assumption)
                    if let MaybeIndex::Some(_) = main_var_id {
                        result.new.push((
                            proc_state.fail(FailureType::Unexpected(format!(
                                "Expected only one declaration of main/0, found another: {}",
                                v
                            ))),
                            "abs_module".to_string(),
                        ));
                    } else {
                        main_var_id = MaybeIndex::Some(*var_id);
                    }
                }

                match &*def.scnd {
                    TypedCore::Fun(_) => {
                        // ... generate a v_addr for the right-hand function (using the left-hand var_name)...
                        let new_v_addr = abstraction.new_vaddr(
                            proc_state,
                            *var_id,
                            &new_item.prog_loc_or_pid,
                            &new_item.env,
                            &new_item.time,
                        );
                        // ... and insert it into the local environment of the next proc_state...
                        new_item.env.inner.insert(*var_id, new_v_addr.clone());

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
                    tc => {
                        result.new.push((
                            proc_state.fail(FailureType::Erlang(format!(
                                "Expected a function, found {}",
                                tc
                            ))),
                            "abs_module".to_string(),
                        ));
                        continue;
                    }
                };
            }
            tc => {
                result.new.push((
                    proc_state.fail(FailureType::Erlang(format!(
                        "Expected a variable name, found {}",
                        tc
                    ))),
                    "abs_module".to_string(),
                ));
                continue;
            }
        }
    }

    // check if main_var_id is set

    match &main_var_id {
        MaybeIndex::Some(main_var_id) => {
            let values = store.unpack(&new_item.env, main_var_id);

            if values.len() != 1 {
                result.new.push((
                    proc_state.fail(FailureType::Unexpected(format!(
                        "Expected only one definition of main/0, found {}",
                        values.len()
                    ))),
                    "abs_module".to_string(),
                ));
            }

            match &values[0] {
                Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                    TypedCore::Fun(f) => match &*f.body {
                        TypedCore::Case(c) => match &c.clauses.inner[0] {
                            TypedCore::Clause(c) => {
                                new_item.prog_loc_or_pid =
                                    ProgLocOrPid::ProgLoc((*c.body).get_index().unwrap());

                                // ... also update the module_env
                                module_env.merge_with(&new_item.env);

                                result.new.push((new_item, "abs_module".to_string()));
                            }
                            tc => {
                                result.new.push((
                                    proc_state.fail(FailureType::Unexpected(format!(
                                        "Expected a clause, found {}",
                                        tc
                                    ))),
                                    "abs_module".to_string(),
                                ));
                            }
                        },
                        tc => {
                            result.new.push((
                                proc_state.fail(FailureType::Unexpected(format!(
                                    "Expected a case statement, found {}",
                                    tc
                                ))),
                                "abs_module".to_string(),
                            ));
                        }
                    },
                    tc => {
                        result.new.push((
                            proc_state.fail(FailureType::Unexpected(format!(
                                "Expected a function, found {}",
                                tc
                            ))),
                            "abs_module".to_string(),
                        ));
                    }
                },
                Value::Pid(pid) => {
                    result.new.push((
                        proc_state.fail(FailureType::Unexpected(format!(
                            "Expected a closure, found pid {}",
                            pid
                        ))),
                        "abs_module".to_string(),
                    ));
                }
            };
        }
        MaybeIndex::None => result.new.push((
            proc_state.fail(FailureType::Unexpected(
                "No main/0 declaration in the environment.".to_string(),
            )),
            "abs_module".to_string(),
        )),
    }

    result
}
