use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_seq, {anno=[] :: list(), arg :: cerl:cerl() | any(), % todo
//		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Seq {
    pub anno: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub body: Box<TypedCore>,
    pub index: Option<usize>,
}

impl From<Map<String, Value>> for Seq {
    fn from(map: Map<String, Value>) -> Self {
        Seq {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
            body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
            index: None,
        }
    }
}
