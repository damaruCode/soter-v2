use crate::ast::*;
use serde_json::{Deserialize, Serialize};

//-record(c_let, {anno=[] :: list(), vars :: [cerl:cerl()],
//		arg :: cerl:cerl(),
//		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Let {
    pub anno: AstList<TypedCore>,
    pub vars: AstList<Var>,
    pub arg: Box<TypedCore>,
    pub body: Box<TypedCore>,
}
