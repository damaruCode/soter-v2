use crate::ast::*;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct AstTuple<T> {
    pub frst: Box<T>,
    pub scnd: Box<T>,
}

impl From<Vec<Value>> for AstTuple<TypedCore> {
    fn from(tuple: Vec<Value>) -> Self {
        assert!(tuple.len() == 2);
        AstTuple {
            frst: Box::new(TypedCore::from(tuple.get(0).unwrap().clone())),
            scnd: Box::new(TypedCore::from(tuple.get(1).unwrap().clone())),
        }
    }
}
