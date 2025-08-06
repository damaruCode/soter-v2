use crate::ast::VarName;

use std::collections::BTreeMap;

use super::ValueAddress;

// Env := Var -> VAddr
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Env<V: ValueAddress> {
    inner: BTreeMap<VarName, V>,
}

impl<V: ValueAddress> Env<V> {
    pub fn init() -> Self {
        Env {
            inner: BTreeMap::new(),
        }
    }

    pub fn get(&self, var_name: &VarName) -> Option<V> {
        self.inner.get(var_name).cloned()
    }
}
