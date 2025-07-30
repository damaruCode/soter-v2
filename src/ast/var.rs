use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_var, {anno=[] :: list(), name :: cerl:var_name()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Var {
    pub anno: AstList<TypedCore>,
    pub name: VarInner,
}
impl From<&Value> for Var {
    fn from(value: &Value) -> Self {
        Var::deserialize(value).unwrap()
    }
}
impl From<Vec<Value>> for AstList<Var> {
    fn from(vv: Vec<Value>) -> Self {
        vv.iter() // turn it into an iterable
            /* and do a fold on it with return type Result<Vec<Var>, ConversionError> */
            .fold(
                AstList::new(), /* base case is Ok( ) on the empty list */
                |mut acc,  /* accumulator */
                 curr_val  /* current element of iterable */| {
                    // if the accumulator is Ok( ) until now, continue conversion
                    acc.push(Var::from(curr_val));
                    acc
                },
            )
    }
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub enum VarInner {
    String(String),
    Number(Number),
}
impl TryFrom<serde_json::Value> for VarInner {
    type Error = ConversionError;

    fn try_from(value: serde_json::Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(s) => Ok(VarInner::String(s.clone())),
            Value::Number(n) => Ok(VarInner::Number(n.clone())),
            _ => Err(ConversionError),
        }
    }
}
