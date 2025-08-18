use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_primop, {anno=[] :: list(), name :: cerl:cerl(),
//		   args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct PrimOp {
    pub anno: AstList<TypedCore>,
    pub name: Box<TypedCore>,
    pub args: AstList<TypedCore>, // NOTE We could probably be more precise here; rogers2018
    // enforces that PrimOp is always applied to names
    pub index: MaybeIndex,
}

impl From<Map<String, Value>> for PrimOp {
    fn from(map: Map<String, Value>) -> Self {
        PrimOp {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            name: Box::new(TypedCore::from(map.get("name").unwrap().clone())),
            args: AstList::from(map.get("args").unwrap().as_array().unwrap().clone()),
            index: MaybeIndex::None,
        }
    }
}

impl Display for PrimOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}prim_op {}({})", self.index, *self.name, self.args)
    }
}
