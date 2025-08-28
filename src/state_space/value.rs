use std::fmt::Display;

use super::{Closure, Pid, ValueAddress};

// Value := Closure U+ Pid
#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Value<V: ValueAddress> {
    Closure(Closure<V>),
    Pid(Pid),
}

impl<V: ValueAddress> Display for Value<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Value::Closure(clo) => write!(f, "{}", clo),
            Value::Pid(pid) => write!(f, "{}", pid),
        }
    }
}
