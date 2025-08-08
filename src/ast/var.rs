use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_var, {anno=[] :: list(), name :: cerl:var_name()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Var {
    pub anno: AstList<TypedCore>,
    pub name: VarName,
}
impl From<Value> for Var {
    fn from(value: Value) -> Self {
        Var::deserialize(value).unwrap()
    }
}
impl From<Map<String, Value>> for Var {
    fn from(map: Map<String, Value>) -> Self {
        Var {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            name: match VarName::try_from(map.get("name").unwrap().clone()) {
                Ok(v) => v,
                Err(e) => panic!("{:?}", e),
            },
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
impl PartialOrd for Var {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.name.partial_cmp(&other.name)
    }
}
impl Ord for Var {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone, PartialOrd, Ord)]
pub struct VarName {
    inner: String,
}

impl From<Value> for VarName {
    fn from(_value: Value) -> Self {
        Self {
            inner: String::new(),
        }
    }
}
impl From<AstList<Var>> for Vec<VarName> {
    fn from(ast_list: AstList<Var>) -> Self {
        let mut vec = Vec::new();
        for var in ast_list.as_vec() {
            vec.push(var.name.clone());
        }
        vec
    }
}
