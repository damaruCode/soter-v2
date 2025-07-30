use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_bitstr, {anno=[] :: list(), val :: cerl:cerl(),
//		   size :: cerl:cerl(),
//		   unit :: cerl:cerl(),
//		   type :: cerl:cerl(),
//		   flags :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct BitStr {
    pub anno: AstList<TypedCore>,
    pub val: Box<TypedCore>,
    pub size: Box<TypedCore>,
    pub unit: Box<TypedCore>,
    pub r#type: Box<TypedCore>,
    pub flags: Box<TypedCore>,
}
