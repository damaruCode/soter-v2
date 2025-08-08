use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_map_pair, {anno=[] :: list(),
//	       op :: #c_literal{val::'assoc'} | #c_literal{val::'exact'},
//		     key :: any(),              % todo
//		     val :: any()}).            % todo
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct MapPair {
    pub anno: AstList<TypedCore>,
    pub op: Literal,
    pub key: Box<TypedCore>,
    pub val: Box<TypedCore>,
}

impl From<Value> for MapPair {
    fn from(value: Value) -> MapPair {
        MapPair::deserialize(value).unwrap()
    }
}

impl From<Map<String, Value>> for MapPair {
    fn from(map: Map<String, Value>) -> Self {
        MapPair {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().to_vec()),
            op: Literal::from(map.get("op").unwrap().clone()),
            key: Box::new(TypedCore::from(map.get("key").unwrap().clone())),
            val: Box::new(TypedCore::from(map.get("val").unwrap().clone())),
        }
    }
}
