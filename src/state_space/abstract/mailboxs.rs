use std::collections::{HashMap, HashSet};

use super::{Pid, Value};

// Mailbox := P(Value)
#[derive(Clone, Debug, PartialEq)]
pub struct Mailbox<'a> {
    inner: HashSet<Value<'a>>,
}

impl Mailbox<'_> {
    pub fn mmatch(self) {} // TODO
    pub fn enq(self) {} // TODO
}

#[derive(Clone, Debug, PartialEq)]
pub struct Mailboxes<'a> {
    pub inner: HashMap<Pid<'a>, Mailbox<'a>>,
}

impl<'a> Mailboxes<'a> {
    pub fn init() -> Self {
        Mailboxes {
            inner: HashMap::new(),
        }
    }
}
