use crate::ast::*;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct VarMap {
    pub anno: AstList<TypedCore>,
    pub arg: Var,
    pub es: AstList<MapPair>,
    pub is_pat: bool,
}

impl From<Map<String, Value>> for VarMap {
    fn from(map: Map<String, Value>) -> Self {
        VarMap {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().to_vec()),
            arg: Var::from(map.get("arg").unwrap().clone()),
            es: AstList::<MapPair>::from(map.get("es").unwrap().as_array().unwrap().to_vec()),
            is_pat: map.get("is_pat").unwrap().as_bool().unwrap(),
        }
    }
}
