use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_values, {anno=[] :: list(), es :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Values {
    pub anno: AstList<TypedCore>,
    pub es: AstList<TypedCore>,
}
