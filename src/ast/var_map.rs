use crate::ast::*;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct VarMap {
    pub anno: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub es: AstList<TypedCore>,
    pub is_pat: bool,
}

impl From<Map<String, Value>> for VarMap {
    fn from(map: Map<String, Value>) -> Self {
        VarMap {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().to_vec()),
            arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
            es: AstList::from(map.get("es").unwrap().as_array().unwrap().to_vec()),
            is_pat: map.get("is_pat").unwrap().as_bool().unwrap(),
        }
    }
}
