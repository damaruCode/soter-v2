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

impl From<Map<String, Value>> for Call {
    fn from(map: Map<String, Value>) -> Self {
        Call {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            module: Box::new(TypedCore::from(map.get("module").unwrap().clone())),
            name: Box::new(TypedCore::from(map.get("name").unwrap().clone())),
            args: AstList::from(map.get("args").unwrap().as_array().unwrap().clone()),
        }
    }
}
