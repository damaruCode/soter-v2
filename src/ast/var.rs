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

impl From<Vec<Value>> for AstList<Var> {
    fn from(vec: Vec<Value>) -> Self {
        let mut ast_list = AstList::new();
        for value in vec {
            ast_list.push(Var::from(value));
        }
        ast_list
    }
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
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
