use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_receive, {anno=[] :: list(), clauses :: [cerl:cerl()],
//		    timeout :: cerl:cerl(),
//		    action :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Receive {
    pub anno: AstList<TypedCore>,
    pub clauses: AstList<TypedCore>,
    pub timeout: Box<TypedCore>,
    pub action: Box<TypedCore>,
}
