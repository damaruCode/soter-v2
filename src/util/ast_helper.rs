use crate::ast::TypedCore;

pub struct AstHelper<'a> {
    ast: TypedCore,
    references: Vec<&'a TypedCore>,
}

impl<'a> AstHelper<'a> {
    pub fn new(tc: TypedCore) -> Self {
        Self {
            ast: tc,
            references: Vec::new(),
        }
    }

    pub fn get(&self, _index: usize) -> Option<&'a TypedCore> {
        None
    }
}
