use super::{Closure, Pid, ValueAddress};

// Value := Closure U+ Pid
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Value<V: ValueAddress> {
    Closure(Closure<V>),
    Pid(Pid),
}
