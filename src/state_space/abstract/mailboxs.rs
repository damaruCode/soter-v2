use std::collections::{HashMap, HashSet};

use super::{Pid, Value, ValueAddress};

// Mailbox := P(Value)
#[derive(Clone, Debug, PartialEq)]
pub struct Mailbox<'a, V: ValueAddress> {
    inner: HashSet<Value<'a, V>>,
}

impl<V: ValueAddress> Mailbox<'_, V> {
    pub fn mmatch(self) {} // TODO
    pub fn enq(self) {} // TODO
}

#[derive(Clone, Debug, PartialEq)]
pub struct Mailboxes<'a, V: ValueAddress> {
    pub inner: HashMap<Pid<'a>, Mailbox<'a, V>>,
}

impl<'a, V: ValueAddress> Mailboxes<'a, V> {
    pub fn init() -> Self {
        Mailboxes {
            inner: HashMap::new(),
        }
    }
}
