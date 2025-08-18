use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_opaque, {anno=[] :: list(), val :: any()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Opaque {
    pub anno: AstList<TypedCore>,
    pub val: Box<TypedCore>,
    pub index: MaybeIndex,
}

impl From<Map<String, Value>> for Opaque {
    fn from(map: Map<String, Value>) -> Self {
        Opaque {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            val: Box::new(TypedCore::from(map.get("val").unwrap().clone())),
            index: MaybeIndex::None,
        }
    }
}

impl Display for Opaque {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
