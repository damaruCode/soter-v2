use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_catch, {anno=[] :: list(), body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Catch {
    pub anno: AstList<TypedCore>,
    pub body: Box<TypedCore>,
}
impl From<Map<String, Value>> for Catch {
    fn from(map: Map<String, Value>) -> Self {
        Catch {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
        }
    }
}
