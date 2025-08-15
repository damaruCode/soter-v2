use crate::{
    state_space::r#abstract::{Env, Value, ValueAddress, VarName},
    util::SetMap,
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
        clauses: Vec<Clause>,
        _env: Env<V>,
        _value_store: &SetMap<V, Value<V>>,
    ) -> Option<(usize, Env<V>)> {
        for clause in clauses {
            if clause.pats.inner.len() == 0 {
                return None;
            }
            for _pat in clause.pats.inner {
                todo!("cmatch should pass to pmatch here")
            }
        }

        None
    }

    pub fn pmatch<V: ValueAddress>(
        _var_name: VarName,
        _v_addr: V,
        _value_store: &SetMap<V, Value<V>>,
    ) -> Env<V> {
        todo!("Pattern matching is not implemented yet")
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
