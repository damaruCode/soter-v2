use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_binary, {anno=[] :: list(), segments :: [cerl:c_bitstr()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Binary {
    pub anno: AstList<TypedCore>,
    pub segments: AstList<BitStr>,
}

impl From<Map<String, Value>> for Binary {
    fn from(map: Map<String, Value>) -> Self {
        Binary {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            segments: AstList::from(map.get("segments").unwrap().as_array().unwrap().clone()),
        }
    }
}
