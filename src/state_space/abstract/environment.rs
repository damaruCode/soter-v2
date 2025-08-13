use crate::ast::Var;

use std::collections::HashMap;

use super::ValueAddress;

// Env := Var -> VAddr
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Env<V: ValueAddress> {
    pub inner: HashMap<Var, V>,
}

impl<V: ValueAddress> Env<V> {
    pub fn init() -> Self {
        Env {
            inner: HashMap::new(),
        }
    }
}
