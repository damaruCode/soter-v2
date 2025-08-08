use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_receive, {anno=[] :: list(), clauses :: [cerl:cerl()],
//		    timeout :: cerl:cerl(),
//		    action :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Receive {
    pub anno: AstList<TypedCore>,
    pub clauses: AstList<TypedCore>,
    pub timeout: Box<TypedCore>,
    pub action: Box<TypedCore>,
}

impl From<Map<String, Value>> for Receive {
    fn from(map: Map<String, Value>) -> Self {
        Receive {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            clauses: AstList::from(map.get("clauses").unwrap().as_array().unwrap().clone()),
            timeout: Box::new(TypedCore::from(map.get("timeout").unwrap().clone())),
            action: Box::new(TypedCore::from(map.get("action").unwrap().clone())),
        }
    }
}
