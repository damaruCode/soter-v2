use crate::ast::*;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct AstList<T> {
    inner: Vec<T>,
}

impl<T> AstList<T> {
    pub fn new() -> Self {
        AstList { inner: Vec::new() }
    }

    pub fn push(&mut self, value: T) {
        self.inner.push(value);
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.inner.get(index)
    }

    pub fn as_vec(&self) -> &Vec<T> {
        &self.inner
    }
}
impl From<Vec<Value>> for AstList<TypedCore> {
    fn from(vec: Vec<Value>) -> AstList<TypedCore> {
        let mut list = Vec::new();
        for val in vec {
            list.push(TypedCore::from(val));
        }
        AstList { inner: list }
    }
}
impl From<Vec<Value>> for AstList<AstTuple<TypedCore>> {
    fn from(vec: Vec<Value>) -> AstList<AstTuple<TypedCore>> {
        let mut list = Vec::new();
        for val in vec {
            list.push(AstTuple::from(val.as_array().unwrap().to_vec()));
        }
        AstList { inner: list }
    }
}
impl From<Vec<Value>> for AstList<BitStr> {
    fn from(vec: Vec<Value>) -> AstList<BitStr> {
        let mut list = Vec::new();
        for val in vec {
            list.push(BitStr::from(val));
        }
        AstList { inner: list }
    }
}
impl From<Vec<Value>> for AstList<MapPair> {
    fn from(vec: Vec<Value>) -> AstList<MapPair> {
        let mut list = Vec::new();
        for val in vec {
            list.push(MapPair::from(val));
        }
        AstList { inner: list }
    }
}
