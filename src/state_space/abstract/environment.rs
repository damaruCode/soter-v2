use super::VAddr;
use crate::ast::Var;
use std::collections::BTreeMap;

// Env := Var -> VAddr
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Env<'a> {
    inner: BTreeMap<Var, VAddr<'a>>,
}

impl Env<'_> {
    pub fn init() -> Self {
        Env {
            inner: BTreeMap::new(),
        }
    }

    // TODO fix Ord problem
    //pub fn get(&self, var: &Var) -> Option<VAddr> {
    //    self.inner.get(var).cloned()
    //}
}
