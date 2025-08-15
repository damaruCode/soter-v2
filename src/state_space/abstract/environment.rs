use std::collections::HashMap;

use super::ValueAddress;
use super::VarName;

// Env := Var -> VAddr
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Env<V: ValueAddress> {
    pub inner: HashMap<VarName, V>,
}

impl<V: ValueAddress> Env<V> {
    pub fn init() -> Self {
        Env {
            inner: HashMap::new(),
        }
    }

    pub fn merge_with(&mut self, other: &Self) {
        for (var_name, v_addr) in &other.inner {
            self.inner.insert(var_name.clone(), v_addr.clone());
        }
    }
}
