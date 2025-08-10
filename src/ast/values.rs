use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_values, {anno=[] :: list(), es :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Values {
    pub anno: AstList<TypedCore>,
    pub es: AstList<TypedCore>,
}

impl From<Map<String, Value>> for Values {
    fn from(map: Map<String, Value>) -> Self {
        Values {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            es: AstList::from(map.get("es").unwrap().as_array().unwrap().clone()),
        }
    }
}
