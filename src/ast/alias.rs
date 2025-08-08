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

impl From<Map<String, Value>> for Alias {
    fn from(map: Map<String, Value>) -> Self {
        Alias {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            var: Box::new(TypedCore::from(map.get("var").unwrap().clone())),
            pat: Box::new(TypedCore::from(map.get("pat").unwrap().clone())),
        }
    }
}
