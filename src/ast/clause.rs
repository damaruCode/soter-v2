use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_clause, {anno=[] :: list(), pats :: [cerl:cerl()],
//		   guard :: cerl:cerl(),
//		   body :: cerl:cerl() | any()}). % todo
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Clause {
    pub anno: AstList<TypedCore>,
    pub pats: AstList<TypedCore>,
    pub guard: Box<TypedCore>,
    pub body: Box<TypedCore>,
}

impl From<Map<String, Value>> for Clause {
    fn from(map: Map<String, Value>) -> Self {
        Clause {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            pats: AstList::from(map.get("pats").unwrap().as_array().unwrap().clone()),
            body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
            guard: Box::new(TypedCore::from(map.get("guard").unwrap().clone())),
        }
    }
}
