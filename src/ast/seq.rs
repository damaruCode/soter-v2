use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_seq, {anno=[] :: list(), arg :: cerl:cerl() | any(), % todo
//		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Seq {
    pub anno: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub body: Box<TypedCore>,
}
