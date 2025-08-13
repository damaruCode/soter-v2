use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_let, {anno=[] :: list(), vars :: [cerl:cerl()],
//		arg :: cerl:cerl(),
//		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Let {
    pub anno: AstList<TypedCore>,
    pub vars: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub body: Box<TypedCore>,
}

impl From<Map<String, Value>> for Let {
    fn from(map: Map<String, Value>) -> Self {
        Let {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            vars: AstList::from(map.get("vars").unwrap().as_array().unwrap().clone()),
            arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
            body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
        }
    }
}
