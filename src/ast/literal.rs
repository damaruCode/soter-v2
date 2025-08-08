use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_literal, {anno=[] :: list(), val :: any()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Literal {
    pub anno: AstList<TypedCore>,
    pub val: Box<TypedCore>,
}

impl From<Value> for Literal {
    fn from(value: Value) -> Literal {
        dbg!(&value);
        Literal::deserialize(value).unwrap()
    }
}

impl From<Map<String, Value>> for Literal {
    fn from(map: Map<String, Value>) -> Self {
        dbg!(&map);
        Literal {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            val: Box::new(TypedCore::from(map.get("val").unwrap().clone())),
        }
    }
}
