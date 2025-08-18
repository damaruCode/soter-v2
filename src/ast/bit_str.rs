use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_bitstr, {anno=[] :: list(), val :: cerl:cerl(),
//		   size :: cerl:cerl(),
//		   unit :: cerl:cerl(),
//		   type :: cerl:cerl(),
//		   flags :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct BitStr {
    pub anno: AstList<TypedCore>,
    pub val: Box<TypedCore>,
    pub size: Box<TypedCore>,
    pub unit: Box<TypedCore>,
    pub r#type: Box<TypedCore>,
    pub flags: Box<TypedCore>,
    pub index: MaybeIndex,
}

impl From<Map<String, Value>> for BitStr {
    fn from(map: Map<String, Value>) -> Self {
        BitStr {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            val: Box::new(TypedCore::from(map.get("val").unwrap().clone())),
            size: Box::new(TypedCore::from(map.get("size").unwrap().clone())),
            unit: Box::new(TypedCore::from(map.get("unit").unwrap().clone())),
            r#type: Box::new(TypedCore::from(map.get("type").unwrap().clone())),
            flags: Box::new(TypedCore::from(map.get("flags").unwrap().clone())),
            index: MaybeIndex::None,
        }
    }
}
impl Display for BitStr {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
