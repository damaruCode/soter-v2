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

impl From<Map<String, Value>> for Cons {
    fn from(map: Map<String, Value>) -> Self {
        Cons {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            hd: Box::new(TypedCore::from(map.get("hd").unwrap().clone())),
            tl: Box::new(TypedCore::from(map.get("tl").unwrap().clone())),
        }
    }
}
