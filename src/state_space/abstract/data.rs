use crate::ast::TypedCore;

// NOTE the free vars of the TypedCore are replaced with the values of the higher scopes and has therefore no
// free vars anymore
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Data<'a> {
    inner: &'a TypedCore,
}
