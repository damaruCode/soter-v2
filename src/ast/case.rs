use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_case, {anno=[] :: list(), arg :: cerl:cerl(),
//		 clauses :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Case {
    pub anno: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub clauses: AstList<TypedCore>,
    pub index: Option<usize>,
}

impl From<Map<String, Value>> for Case {
    fn from(map: Map<String, Value>) -> Self {
        Case {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
            clauses: AstList::from(map.get("clauses").unwrap().as_array().unwrap().clone()),
            index: None,
        }
    }
}
