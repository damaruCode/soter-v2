use crate::ast::*;
use serde_json::{Deserialize, Serialize};

//-record(c_try, {anno=[] :: list(), arg :: cerl:cerl(),
//		vars :: [cerl:cerl()],
//		body :: cerl:cerl(),
//		evars :: [cerl:cerl()],
//		handler :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Try {
    pub anno: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub vars: AstList<TypedCore>,
    pub body: Box<TypedCore>,
    pub evars: AstList<TypedCore>,
    pub handler: Box<TypedCore>,
}
