use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_map_pair, {anno=[] :: list(),
//	       op :: #c_literal{val::'assoc'} | #c_literal{val::'exact'},
//		     key :: any(),              % todo
//		     val :: any()}).            % todo
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
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
