use super::{ValueAddress, Var};
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

    pub fn get(&self, var: &Var) -> Option<V> {
        self.inner.get(var).cloned()
    }
}
