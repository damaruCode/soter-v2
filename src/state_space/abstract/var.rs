use crate::ast::Var as AstVar;

#[derive(Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct Var<'a> {
    inner: &'a AstVar,
}

impl<'a> Var<'a> {
    pub fn new(ast_var: &'a AstVar) -> Self {
        Var { inner: ast_var }
    }
}

impl<'a> PartialEq for Var<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.name == other.inner.name
    }
}
