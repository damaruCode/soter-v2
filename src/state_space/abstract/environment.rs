use crate::ast::VarName;

use super::VAddr;
use std::collections::BTreeMap;

// Env := Var -> VAddr
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Env<'a> {
    inner: BTreeMap<VarName, VAddr<'a>>,
}

impl Env<'_> {
    pub fn init() -> Self {
        Env {
            inner: BTreeMap::new(),
        }
    }

    pub fn get(&self, var_name: &VarName) -> Option<VAddr> {
        self.inner.get(var_name).cloned()
    }
}
