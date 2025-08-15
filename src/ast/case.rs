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
    //TODO
    pub fn cmatch<V: ValueAddress>(
        clauses: Vec<Clause>,
        v_addr: V,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Option<(usize, Env<V>)> {
        for i in 0..clauses.len() {
            if clauses[i].pats.inner.len() == 0 {
                return None;
            }
            let mut new_env = Env::init();
            for pat in &clauses[i].pats.inner {
                let p_env = Self::pmatch(pat, v_addr.clone(), value_store, ast_helper);
                if let Some(env) = p_env {
                    new_env.merge_with(&env);
                }
            }
            if Self::gmatch(&clauses[i].guard, &new_env, value_store, ast_helper) {
                return Some((i, new_env));
            }
        }
        None
    }

    //TODO
    pub fn pmatch<V: ValueAddress>(
        typed_core: &TypedCore,
        v_addr: V,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Option<Env<V>> {
        match typed_core {
            TypedCore::AstList(outer_at) => {
                let values = value_store.get(&v_addr).unwrap();
                for value in values {
                    match value {
                        Value::Closure(c) => match ast_helper.get(c.prog_loc) {
                            TypedCore::AstList(inner_at) => {
                                let mut new_env = Env::init();
                                for i in 0..outer_at.inner.len() {
                                    let p_env = Self::pmatch(
                                        &outer_at.inner[i],
                                        c.env
                                            .inner
                                            .get(&VarName::from(&inner_at.inner[i]))
                                            .unwrap()
                                            .clone(),
                                        value_store,
                                        ast_helper,
                                    );

                                    if let Some(env) = p_env {
                                        new_env.merge_with(&env);
                                    }
                                }

                                return Some(new_env);
                            }
                            _ => panic!(),
                        },
                        _ => panic!(),
                    }
                }
            }
            _ => panic!(),
        };
        None
    }

    //TODO
    pub fn gmatch<V: ValueAddress>(
        typed_core: &TypedCore,
        env: &Env<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> bool {
        todo!("Guard matching is not implemented yet")
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
