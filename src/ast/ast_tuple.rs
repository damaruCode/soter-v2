use crate::ast::*;
use serde::{Deserialize, Serialize};
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct AstTuple<T> {
    pub frst: Box<T>,
    pub scnd: Box<T>,
}

impl From<Vec<Value>> for AstTuple<TypedCore> {
    fn from(tuple: Vec<Value>) -> Self {
        assert!(tuple.len() == 2);
        AstTuple {
            frst: Box::new(type_core(tuple.get(0).unwrap().clone())),
            scnd: Box::new(type_core(tuple.get(1).unwrap().clone())),
        }
    }
}
