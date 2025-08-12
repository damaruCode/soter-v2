use std::collections::{HashMap, HashSet};

use super::{Pid, Value, ValueAddress};

// Mailbox := P(Value)
#[derive(Clone, Debug, PartialEq)]
pub struct Mailbox<V: ValueAddress> {
    inner: HashSet<Value<V>>,
}

impl<V: ValueAddress> Mailbox<V> {
    pub fn mmatch(self) {} // TODO
    pub fn enq(self) {} // TODO
}

#[derive(Clone, Debug, PartialEq)]
pub struct Mailboxes<V: ValueAddress> {
    pub inner: HashMap<Pid, Mailbox<V>>,
}

impl<V: ValueAddress> Mailboxes<V> {
    pub fn init() -> Self {
        Mailboxes {
            inner: HashMap::new(),
        }
    }
}
