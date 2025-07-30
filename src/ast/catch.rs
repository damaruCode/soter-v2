use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_catch, {anno=[] :: list(), body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Catch {
    pub anno: AstList<TypedCore>,
    pub body: Box<TypedCore>,
}
