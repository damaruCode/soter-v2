use crate::ast::Fun;

use super::Pid;

// NOTE the free vars of the TypedCore are replaced with the values of the higher scopes and has therefore no
// free vars anymore
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Data<'a> {
    Pid(Pid<'a>),
    Atom(),
    Fun(Fun),
    Constructor(Vec<Data<'a>>),
}
