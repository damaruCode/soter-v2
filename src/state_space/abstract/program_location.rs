use crate::ast::TypedCore;

// NOTE this references the Exps in the Ast
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct ProgLoc<'a> {
    inner: &'a TypedCore,
}

impl<'a> ProgLoc<'a> {
    pub fn new(inner: &'a TypedCore) -> Self {
        ProgLoc { inner }
    }
    pub fn init(ast: &'a TypedCore) -> Self {
        ProgLoc { inner: ast }
    }
    pub fn get(&self) -> &'a TypedCore {
        self.inner
    }
}
