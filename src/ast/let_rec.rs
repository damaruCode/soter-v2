use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_letrec, {anno=[] :: list(),
//       defs :: [{cerl:cerl(), cerl:cerl()}],
//		   body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct LetRec {
    pub anno: AstList<TypedCore>,
    pub defs: AstList<AstTuple<TypedCore>>,
    pub body: Box<TypedCore>,
}
impl From<Map<String, Value>> for LetRec {
    fn from(map: Map<String, Value>) -> Self {
        LetRec {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            defs: AstList::from(map.get("defs").unwrap().as_array().unwrap().clone()),
            body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
        }
    }
}
