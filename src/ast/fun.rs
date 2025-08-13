use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_fun, {anno=[] :: list(), vars :: [cerl:cerl()],
//		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Fun {
    pub anno: AstList<TypedCore>,
    pub vars: AstList<TypedCore>,
    pub body: Box<TypedCore>,
    pub index: Option<usize>,
}

impl From<Map<String, Value>> for Fun {
    fn from(map: Map<String, Value>) -> Self {
        Fun {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            vars: AstList::from(map.get("vars").unwrap().as_array().unwrap().clone()),
            body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
            index: None,
        }
    }
}
