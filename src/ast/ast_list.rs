use crate::ast::*;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct AstList<T> {
    pub inner: Vec<T>,
    pub index: MaybeIndex,
}

impl<T> AstList<T> {
    pub fn new() -> Self {
        AstList {
            inner: Vec::new(),
            index: MaybeIndex::None,
        }
    }
}

impl From<Vec<Value>> for AstList<TypedCore> {
    fn from(vec: Vec<Value>) -> AstList<TypedCore> {
        let mut list = Vec::new();

        for val in vec {
            list.push(TypedCore::from(val));
        }

        // SPECIAL CASE: literal lists are to be handled as lists of literals
        if list.len() == 1 {
            match &list[0].clone() {
                TypedCore::Literal(l) => match (*l.val).clone() {
                    TypedCore::AstList(al) => {
                        list.clear();
                        for val in al.inner {
                            list.push(TypedCore::Literal(Literal {
                                anno: l.anno.clone(),
                                val: Box::new(val),
                                index: l.index.clone(),
                            }));
                        }
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        AstList {
            inner: list,
            index: MaybeIndex::None,
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
            index: MaybeIndex::None,
        }
    }
}

impl From<Vec<TypedCore>> for AstList<TypedCore> {
    fn from(vec: Vec<TypedCore>) -> AstList<TypedCore> {
        let mut list = Vec::new();
        for val in vec {
            list.push(TypedCore::from(val));
        }
        AstList {
            inner: list,
            index: MaybeIndex::None,
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

impl<T: Display> Display for AstList<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let items: Vec<String> = self.inner.iter().map(|item| format!("{}", item)).collect();
        write!(f, "{}ast_list [{}]", self.index, items.join(", "))
    }
}
