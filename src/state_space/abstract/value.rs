use super::{Closure, Pid};

// Value := Closure U+ Pid
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ClosureOrPid<'a> {
    Closure(Closure<'a>),
    Pid(Pid<'a>),
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Value<'a> {
    inner: ClosureOrPid<'a>,
}
