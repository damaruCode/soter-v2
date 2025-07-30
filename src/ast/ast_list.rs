use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct AstList<T> {
    pub inner: Vec<T>,
}

impl<T> AstList<T> {
    pub fn new() -> Self {
        AstList { inner: Vec::new() }
    }

    pub fn push(&mut self, value: T) {
        self.inner.push(value);
    }
}
