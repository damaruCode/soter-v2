use crate::{
    state_space::{Env, Value, ValueAddress, VarName},
    util::{AstHelper, SetMap},
};
use serde::{Deserialize, Serialize};
use serde_json::Map;

use super::{AstList, Clause, TypedCore};

pub enum VarNamesOrValue<V: ValueAddress> {
    VarNames(Vec<VarName>),
    Value(Value<V>),
}

pub enum ValueAddressOrValue<V: ValueAddress> {
    ValueAddress(V),
    Value(Value<V>),
}

//-record(c_case, {anno=[] :: list(), arg :: cerl:cerl(),
//		 clauses :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Case {
    pub anno: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub clauses: AstList<TypedCore>,
    pub index: Option<usize>,
}

impl Case {
    pub fn cmatch<V: ValueAddress>(
        clauses: &Vec<Clause>,
        v_addr_or_value: &ValueAddressOrValue<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<Option<(usize, Env<V>)>> {
        let mut opts = Vec::new();
        for i in 0..clauses.len() {
            let mut new_env = Env::init();
            let p_env = Self::pmatch(
                &clauses[i].pats.inner[i],
                v_addr_or_value,
                value_store,
                ast_helper,
            );

            if let Some(env) = p_env {
                new_env.merge_with(&env);
                if Self::gmatch(&clauses[i].guard, &new_env, value_store, ast_helper) {
                    opts.push(Some((i, new_env)))
                }
            }
        }
        opts
    }

    //TODO No idea if this is right; doesn't really matter right now
    pub fn pmatch<V: ValueAddress>(
        typed_core: &TypedCore,
        v_addr_or_value: &ValueAddressOrValue<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Option<Env<V>> {
        fn traverse<V: ValueAddress>(
            ast_list: &AstList<TypedCore>,
            v_addr_or_value: &ValueAddressOrValue<V>,
            value_store: &SetMap<V, Value<V>>,
            ast_helper: &AstHelper,
        ) -> Option<Env<V>> {
            let values = match v_addr_or_value {
                ValueAddressOrValue::Value(value) => Vec::from([value.clone()]),
                ValueAddressOrValue::ValueAddress(v_addr) => {
                    value_store.get(&v_addr).unwrap().clone()
                }
            };

            let mut new_env = Env::init();
            for value in values {
                match value {
                    Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                        TypedCore::AstList(var_al) => {
                            for i in 0..ast_list.inner.len() {
                                let p_env = Case::pmatch(
                                    &ast_list.inner[i],
                                    &ValueAddressOrValue::ValueAddress(
                                        clo.env
                                            .inner
                                            .get(&VarName::from(&var_al.inner[i]))
                                            .unwrap()
                                            .clone(),
                                    ),
                                    value_store,
                                    ast_helper,
                                );

                                match p_env {
                                    Some(env) => new_env.merge_with(&env),
                                    None => return None,
                                }
                            }
                        }
                        _ => return None, // TODO same as below with Pid
                    },
                    Value::Pid(_) => return None, // TODO think about this / maybe talk about it; right now
                                                  // I am of the impression, that this can't match because we
                                                  // are comparing it to an ast_list (pid != ast_list)
                };
            }
            Some(new_env)
        }

        match typed_core {
            TypedCore::Var(v) => match v_addr_or_value {
                ValueAddressOrValue::ValueAddress(v_addr) => {
                    let mut new_env = Env::init();
                    new_env
                        .inner
                        .insert(VarName::from(v).clone(), v_addr.clone());

                    Some(new_env)
                }
                _ => panic!(),
            },
            TypedCore::String(s1) => match v_addr_or_value {
                ValueAddressOrValue::ValueAddress(v_addr) => match value_store.get(&v_addr) {
                    Some(_) => Some(Env::init()),
                    None => None,
                },
                ValueAddressOrValue::Value(val) => match val {
                    Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                        TypedCore::String(s2) => {
                            if s1.inner == s2.inner {
                                Some(Env::init())
                            } else {
                                None
                            }
                        }
                        _ => panic!(),
                    },
                    _ => panic!(),
                },
            },
            TypedCore::AstList(al) => traverse(al, v_addr_or_value, value_store, ast_helper),
            TypedCore::Tuple(tup) => traverse(&tup.es, v_addr_or_value, value_store, ast_helper),
            _ => panic!("{:#?}", typed_core),
        }
    }

    pub fn gmatch<V: ValueAddress>(
        typed_core: &TypedCore,
        _env: &Env<V>,
        _value_store: &SetMap<V, Value<V>>,
        _ast_helper: &AstHelper,
    ) -> bool {
        match typed_core {
            TypedCore::Literal(l) => match *l.val.clone() {
                TypedCore::String(s) => {
                    if s.inner.as_str() == "true" {
                        true
                    } else {
                        todo!("{:#?}", typed_core)
                    }
                }
                TypedCore::Bool(b) => b.inner,
                _ => todo!("{:#?}", typed_core),
            },
            _ => todo!("{:#?}", typed_core),
        }
    }
}

impl From<Map<String, serde_json::Value>> for Case {
    fn from(map: Map<String, serde_json::Value>) -> Self {
        Case {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
            clauses: AstList::from(map.get("clauses").unwrap().as_array().unwrap().clone()),
            index: None,
        }
    }
}
