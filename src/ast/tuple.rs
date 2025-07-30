use crate::ast::*;
use serde_json::{Deserialize, Serialize};

//-record(c_tuple, {anno=[] :: list(), es :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Tuple {
    pub anno: AstList<TypedCore>,
    pub es: AstList<TypedCore>,
}
