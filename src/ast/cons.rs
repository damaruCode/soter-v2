use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_cons, {anno=[] :: list(), hd :: cerl:cerl(),
//		 tl :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Cons {
    pub anno: AstList<TypedCore>,
    pub hd: Box<TypedCore>,
    pub tl: Box<TypedCore>,
}
