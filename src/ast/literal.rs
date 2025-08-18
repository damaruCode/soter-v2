use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_literal, {anno=[] :: list(), val :: any()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Literal {
    pub anno: AstList<TypedCore>,
    pub val: Box<TypedCore>,
    pub index: MaybeIndex,
}

impl From<Map<String, Value>> for Literal {
    fn from(map: Map<String, Value>) -> Self {
        Literal {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            val: Box::new(TypedCore::from(map.get("val").unwrap().clone())),
            index: MaybeIndex::None,
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}literal {}", self.index, *self.val)
    }
}
