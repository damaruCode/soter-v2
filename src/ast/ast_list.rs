use crate::ast::*;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct AstList<T> {
    pub inner: Vec<T>,
    pub index: Option<usize>,
}

impl<T> AstList<T> {
    pub fn new() -> Self {
        AstList {
            inner: Vec::new(),
            index: None,
        }
    }
}

impl From<Vec<Value>> for AstList<TypedCore> {
    fn from(vec: Vec<Value>) -> AstList<TypedCore> {
        let mut list = Vec::new();
        for val in vec {
            list.push(TypedCore::from(val));
        }
        AstList {
            inner: list,
            index: None,
        }
    }
}

impl From<Vec<Value>> for AstList<AstTuple<TypedCore>> {
    fn from(vec: Vec<Value>) -> AstList<AstTuple<TypedCore>> {
        let mut list = Vec::new();
        for val in vec {
            list.push(AstTuple::from(val.as_array().unwrap().to_vec()));
        }
        AstList {
            inner: list,
            index: None,
        }
    }
}

impl From<&AstList<TypedCore>> for Vec<usize> {
    fn from(al: &AstList<TypedCore>) -> Self {
        let mut vec = Vec::new();
        for tc in &al.inner {
            vec.push(tc.get_index().unwrap());
        }
        vec
    }
}
