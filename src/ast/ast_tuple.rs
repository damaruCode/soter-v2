use crate::ast::*;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct AstTuple<T> {
    pub frst: Box<T>,
    pub scnd: Box<T>,
    pub index: MaybeIndex,
}

impl From<Vec<Value>> for AstTuple<TypedCore> {
    fn from(tuple: Vec<Value>) -> Self {
        assert!(tuple.len() == 2);
        AstTuple {
            frst: Box::new(TypedCore::from(tuple.get(0).unwrap().clone())),
            scnd: Box::new(TypedCore::from(tuple.get(1).unwrap().clone())),
            index: MaybeIndex::None,
        }
    }
}

impl From<&AstTuple<TypedCore>> for (usize, usize) {
    fn from(at: &AstTuple<TypedCore>) -> Self {
        (at.frst.get_index().unwrap(), at.scnd.get_index().unwrap())
    }
}

impl<T: Display> Display for AstTuple<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}ast_tuple ({}, {})",
            self.index, *self.frst, *self.scnd
        )
    }
}
