use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_module, {anno=[] :: list(), name :: cerl:cerl(),
//		   exports :: [cerl:cerl()],
//		   attrs :: [{cerl:cerl(), cerl:cerl()}],
//		   defs :: [{cerl:cerl(), cerl:cerl()}]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Module {
    pub anno: AstList<TypedCore>,
    pub name: Box<TypedCore>,
    pub exports: AstList<TypedCore>,
    pub attrs: AstList<AstTuple<TypedCore>>,
    pub defs: AstList<AstTuple<TypedCore>>,
}

impl From<Map<String, Value>> for Module {
    fn from(map: Map<String, Value>) -> Self {
        Module {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().to_vec()),
            name: Box::new(TypedCore::from(map.get("name").unwrap().clone())),
            exports: AstList::from(map.get("exports").unwrap().as_array().unwrap().to_vec()),
            attrs: AstList::from(map.get("attrs").unwrap().as_array().unwrap().to_vec()),
            defs: AstList::from(map.get("defs").unwrap().as_array().unwrap().to_vec()),
        }
    }
}
