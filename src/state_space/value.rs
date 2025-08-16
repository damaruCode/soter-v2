use super::{Closure, Pid, ValueAddress};

// Value := Closure U+ Pid
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value<V: ValueAddress> {
    Closure(Closure<V>),
    Pid(Pid),
}
