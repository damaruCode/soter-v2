use crate::{
    ast::{Case, Clause},
    util::SetMap,
};

use super::{Env, Pid, Value, ValueAddress};
use std::collections::HashMap;

// Mailbox := P(Value)
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Mailbox<V: ValueAddress> {
    pub inner: Vec<Value<V>>,
}

impl<V: ValueAddress> Mailbox<V> {
    // TODO
    pub fn mmatch(
        &self,
        clauses: Vec<Clause>,
        value_store: &SetMap<V, Value<V>>,
    ) -> Option<(usize, V, Env<V>)> {
        None
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Mailboxes<V: ValueAddress> {
    pub inner: HashMap<Pid, Mailbox<V>>,
}

impl<V: ValueAddress> Mailboxes<V> {
    pub fn init() -> Self {
        Mailboxes {
            inner: HashMap::new(),
        }
    }

    pub fn push(&mut self, pid: Pid, value: Value<V>) {
        match self.inner.get_mut(&pid) {
            Some(mb) => {
                if !mb.inner.contains(&value) {
                    mb.inner.push(value);
                }
            }
            None => {
                let _ = self.inner.insert(pid, Mailbox { inner: vec![value] });
            }
        }
    }
}
