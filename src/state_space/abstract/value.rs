use super::{Closure, Pid, ValueAddress};

// Value := Closure U+ Pid
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ClosureOrPid<'a, V: ValueAddress> {
    Closure(Closure<'a, V>),
    Pid(Pid<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Value<'a, V: ValueAddress> {
    inner: ClosureOrPid<'a, V>,
}
