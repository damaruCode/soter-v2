use crate::ast::VarName;

use super::VAddr;

use std::collections::BTreeMap;

// Env := Var -> VAddr
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Env<'a, V: ValueAddress> {
    inner: BTreeMap<Var<'a>, V>,
}

impl<V: ValueAddress> Env<'_, V> {
    pub fn init() -> Self {
        Env {
            inner: BTreeMap::new(),
        }
    }

    pub fn get(&self, var_name: &VarName) -> Option<V> {
        self.inner.get(var_name).cloned()
    }
}
