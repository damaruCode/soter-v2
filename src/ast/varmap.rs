use crate::ast::*;
use serde_json::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct VarMap {
    pub anno: AstList<TypedCore>,
    pub arg: Var,
    pub es: AstList<MapPair>,
    pub is_pat: bool,
}
