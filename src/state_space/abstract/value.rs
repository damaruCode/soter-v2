use super::{Closure, Pid, ValueAddress};

// Value := Closure U+ Pid
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Value<'a, V: ValueAddress> {
    Closure(Closure<'a, V>),
    Pid(Pid<'a>),
}
