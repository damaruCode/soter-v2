use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_call, {anno=[] :: list(), module :: cerl:cerl(),
//		 name :: cerl:cerl(),
//		 args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Call {
    pub anno: AstList<TypedCore>,
    pub module: Box<TypedCore>,
    pub name: Box<TypedCore>,
    pub args: AstList<TypedCore>,
}
