use super::{Pid, Value, ValueAddress};

// Mailbox := P(Value)
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Mailbox<V: ValueAddress> {
    inner: Vec<Value<V>>,
}

impl<V: ValueAddress> Mailbox<V> {
    pub fn mmatch(self) {} // TODO
    pub fn enq(self) {} // TODO
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Mailboxes<V: ValueAddress> {
    pub inner: Vec<(Pid, Mailbox<V>)>,
}

impl<V: ValueAddress> Mailboxes<V> {
    pub fn init() -> Self {
        Mailboxes { inner: Vec::new() }
    }
}
