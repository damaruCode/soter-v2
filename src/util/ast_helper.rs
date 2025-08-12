use crate::ast::TypedCore;

pub struct AstHelper {
    ast: TypedCore,
}

impl AstHelper {
    pub fn new(tc: TypedCore) -> Self {
        Self { ast: tc }
    }

    pub fn get(&self, _index: usize) -> Option<&TypedCore> {
        Some(&self.ast)
    }
}
