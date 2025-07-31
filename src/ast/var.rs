use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_var, {anno=[] :: list(), name :: cerl:var_name()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Var {
    pub anno: AstList<TypedCore>,
    pub name: VarInner,
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
            name: match VarInner::try_from(map.get("name").unwrap().clone()) {
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
        match &self.name {
            VarInner::String(s1) => match &other.name {
                VarInner::String(s2) => Some(s1.cmp(s2)),
                VarInner::Number(_n2) => Some(std::cmp::Ordering::Less),
            },
            VarInner::Number(n1) => match &other.name {
                VarInner::String(_s2) => Some(std::cmp::Ordering::Greater),
                VarInner::Number(n2) => Some(n1.as_i64().unwrap().cmp(&n2.as_i64().unwrap())),
            },
        }
    }
}
impl Ord for Var {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub enum VarInner {
    String(String),
    Number(Number),
}

impl From<Value> for VarInner {
    fn from(value: Value) -> Self {
        match value {
            Value::String(s) => VarInner::String(s),
            Value::Number(n) => VarInner::Number(n),
            _ => panic!("Unexpected type for VarInner"),
        }
    }
}
impl From<AstList<Var>> for Vec<VarInner> {
    fn from(ast_list: AstList<Var>) -> Self {
        let mut vec = Vec::new();
        for var in ast_list.inner {
            vec.push(var.name);
        }
        vec
    }
}
