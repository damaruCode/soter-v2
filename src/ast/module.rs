use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_module, {anno=[] :: list(), name :: cerl:cerl(),
//		   exports :: [cerl:cerl()],
//		   attrs :: [{cerl:cerl(), cerl:cerl()}],
//		   defs :: [{cerl:cerl(), cerl:cerl()}]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Module {
    pub anno: AstList<TypedCore>,
    pub name: Box<TypedCore>,
    pub exports: AstList<TypedCore>,
    pub attrs: AstList<AstTuple<TypedCore>>,
    pub defs: AstList<AstTuple<TypedCore>>,
}
