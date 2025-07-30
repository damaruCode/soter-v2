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
impl From<Map<String, Value>> for PrimOp {
    fn from(map: Map<String, Value>) -> Self {
        PrimOp {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            name: Box::new(TypedCore::from(map.get("name").unwrap().clone())),
            args: AstList::from(map.get("args").unwrap().as_array().unwrap().clone()),
        }
    }
}
