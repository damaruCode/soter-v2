use super::Pid;
use crate::ast::Fun;

// NOTE the free vars of the TypedCore are replaced with the values of the higher scopes and has therefore no
// free vars anymore
// TODO
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Data {
    Pid(Pid),
    Atom(),
    Fun(Fun),
    Constructor(Vec<Data>),
}
