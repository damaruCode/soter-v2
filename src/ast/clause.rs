use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_clause, {anno=[] :: list(), pats :: [cerl:cerl()],
//		   guard :: cerl:cerl(),
//		   body :: cerl:cerl() | any()}). % todo
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Clause {
    pub anno: AstList<TypedCore>,
    pub pats: AstList<TypedCore>,
    pub guard: Box<TypedCore>,
    pub body: Box<TypedCore>,
}
