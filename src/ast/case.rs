use crate::{
    state_space::r#abstract::{Env, Value, ValueAddress, VarName},
    util::{AstHelper, SetMap},
};
use serde::{Deserialize, Serialize};
use serde_json::Map;

use super::{AstList, Clause, TypedCore};

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
        values: &Vec<Value<V>>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<Option<(usize, Env<V>)>> {
        let mut opts = Vec::new();
        for i in 0..clauses.len() {
            let mut new_env = Env::init();
            for pat in &clauses[i].pats.inner {
                let p_envs = Self::pmatch(pat, values, value_store, ast_helper);

                for p_env in p_envs {
                    if let Some(env) = p_env {
                        new_env.merge_with(&env);
                    }
                }
            }
            if Self::gmatch(&clauses[i].guard, &new_env, value_store, ast_helper) {
                opts.push(Some((i, new_env)));
            }
        }
        opts
    }

    //TODO No idea if this is right; doesn't really matter right now
    pub fn pmatch<V: ValueAddress>(
        typed_core: &TypedCore,
        values: &Vec<Value<V>>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<Option<Env<V>>> {
        let mut opts = Vec::new();
        match typed_core {
            TypedCore::AstList(tc_al) => {
                for value in values {
                    match value {
                        Value::Closure(c) => match ast_helper.get(c.prog_loc) {
                            TypedCore::AstList(var_al) => {
                                let mut new_env = Env::init();
                                for i in 0..tc_al.inner.len() {
                                    let p_envs = Self::pmatch(
                                        &tc_al.inner[i],
                                        value_store
                                            .get(
                                                &c.env
                                                    .inner
                                                    .get(&VarName::from(&var_al.inner[i]))
                                                    .unwrap()
                                                    .clone(),
                                            )
                                            .unwrap(),
                                        value_store,
                                        ast_helper,
                                    );

                                    for p_env in p_envs {
                                        if let Some(env) = p_env {
                                            new_env.merge_with(&env);
                                        }
                                    }
                                }

                                opts.push(Some(new_env));
                            }
                            _ => todo!(),
                        },
                        _ => panic!(),
                    }
                }
            }
            _ => panic!(),
        };
        opts
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
