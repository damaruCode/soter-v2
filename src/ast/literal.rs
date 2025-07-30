use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_literal, {anno=[] :: list(), val :: any()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Literal {
    pub anno: AstList<TypedCore>,
    pub val: Box<TypedCore>,
}

impl From<Value> for Literal {
    fn from(value: Value) -> Literal {
        Literal::deserialize(value).unwrap()
    }
}
