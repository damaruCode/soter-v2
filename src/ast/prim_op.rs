use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_primop, {anno=[] :: list(), name :: cerl:cerl(),
//		   args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct PrimOp {
    pub anno: AstList<TypedCore>,
    pub name: Box<TypedCore>,
    pub args: AstList<TypedCore>, // NOTE We could probably be more precise here; rogers2018 enforces that
                                  // PrimOp is always applied to names
}

impl From<Map<String, Value>> for PrimOp {
    fn from(map: Map<String, Value>) -> Self {
        PrimOp {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            name: Box::new(TypedCore::from(map.get("name").unwrap().clone())),
            args: AstList::from(map.get("args").unwrap().as_array().unwrap().clone()),
        }
    }
}

pub struct SelfOp {
    pub anno: AstList<TypedCore>,
}

impl From<Map<String, Value>> for SelfOp {
    fn from(map: Map<String, Value>) -> Self {
        Self {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
        }
    }
}

pub struct SpawnOp {
    pub anno: AstList<TypedCore>,
    pub arg: TypedCore, // NOTE We could probably be more precise here; rogers2018 enforces that
                        // PrimOp is always applied to names
}
impl From<Map<String, Value>> for SpawnOp {
    fn from(map: Map<String, Value>) -> Self {
        Self {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            arg: TypedCore::from(
                map.get("args")
                    .unwrap()
                    .as_array()
                    .unwrap()
                    .clone()
                    .get(0)
                    .unwrap()
                    .clone(),
            ),
        }
    }
}

pub struct SendOp {
    pub anno: AstList<TypedCore>,
    pub args: (TypedCore, TypedCore), // NOTE We could probably be more precise here; rogers2018 enforces that
                                      // PrimOp is always applied to names
}
impl From<Map<String, Value>> for SendOp {
    fn from(map: Map<String, Value>) -> Self {
        let vec = map.get("args").unwrap().as_array().unwrap();

        Self {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            args: (
                TypedCore::from(vec.get(0).unwrap().clone()),
                TypedCore::from(vec.get(1).unwrap().clone()),
            ),
        }
    }
}
