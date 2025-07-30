use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_primop, {anno=[] :: list(), name :: cerl:cerl(),
//		   args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct PrimOp {
    pub anno: AstList<TypedCore>,
    pub name: Box<TypedCore>,
    pub args: AstList<TypedCore>,
}
