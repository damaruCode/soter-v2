use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_tuple, {anno=[] :: list(), es :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Tuple {
    pub anno: AstList<TypedCore>,
    pub es: AstList<TypedCore>,
}

impl From<Map<String, Value>> for Tuple {
    fn from(map: Map<String, Value>) -> Self {
        Tuple {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            es: AstList::from(map.get("es").unwrap().as_array().unwrap().clone()),
        }
    }
}
