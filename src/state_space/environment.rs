use std::collections::BTreeMap;
use std::fmt::Display;

use super::ValueAddress;

// Env := Var -> VAddr
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Env<V: ValueAddress> {
    pub inner: BTreeMap<usize, V>,
}

impl<V: ValueAddress> Env<V> {
    pub fn init() -> Self {
        Env {
            inner: BTreeMap::new(),
        }
    }

    pub fn merge_with(&mut self, other: &Self) {
        for (var_id, v_addr) in &other.inner {
            self.inner.insert(*var_id, v_addr.clone());
        }
    }
}

impl<V: ValueAddress> Display for Env<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = self
            .inner
            .iter()
            .map(|(var_id, v_addr)| format!("{var_id} |-> {v_addr}"))
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "[{output}]")
    }
}
