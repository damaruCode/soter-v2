use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_alias, {anno=[] :: list(), var :: cerl:cerl(),
//		  pat :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Alias {
    pub anno: AstList<TypedCore>,
    pub var: Box<TypedCore>,
    pub pat: Box<TypedCore>,
}
