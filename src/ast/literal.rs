use crate::ast::*;
use serde_json::{Deserialize, Serialize};

//-record(c_literal, {anno=[] :: list(), val :: any()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Literal {
    pub anno: AstList<TypedCore>,
    pub val: Box<TypedCore>,
}
