use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_var, {anno=[] :: list(), name :: cerl:var_name()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Var {
    pub anno: AstList<TypedCore>,
    pub name: Box<TypedCore>,
}

impl From<Value> for Var {
    fn from(value: Value) -> Self {
        dbg!(&value);
        Var::deserialize(value).unwrap()
    }
}

impl From<Map<String, Value>> for Var {
    fn from(map: Map<String, Value>) -> Self {
        Var {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            name: Box::new(TypedCore::from(map.get("name").unwrap().clone())),
        }
    }
}

impl From<Vec<Value>> for AstList<Var> {
    fn from(vec: Vec<Value>) -> Self {
        let mut ast_list = AstList::new();
        for value in vec {
            ast_list.push(Var::from(value));
        }
        ast_list
    }
}
