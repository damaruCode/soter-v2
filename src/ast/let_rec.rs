use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_letrec, {anno=[] :: list(),
//       defs :: [{cerl:cerl(), cerl:cerl()}],
//		   body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct LetRec {
    pub anno: AstList<TypedCore>,
    pub defs: AstList<AstTuple<TypedCore>>,
    pub body: Box<TypedCore>,
}
