use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_binary, {anno=[] :: list(), segments :: [cerl:c_bitstr()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Binary {
    pub anno: AstList<TypedCore>,
    pub segments: AstList<BitStr>,
}
