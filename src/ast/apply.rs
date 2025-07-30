use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_apply, {anno=[] :: list(), op :: cerl:cerl(),
//		  args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Apply {
    pub anno: AstList<TypedCore>,
    pub op: Box<TypedCore>,
    pub args: AstList<TypedCore>,
}
