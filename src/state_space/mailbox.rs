use crate::{
    ast::{Case, Clause, ValueAddressOrValue},
    util::{AstHelper, SetMap},
};

use super::{Env, Pid, Value, ValueAddress};
use std::collections::BTreeMap;

/// Mailbox := P(Value),
/// in this case this represents the AbsMailbox_set abstraction
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Mailbox<V: ValueAddress> {
    pub inner: Vec<Value<V>>,
}

impl<V: ValueAddress> Mailbox<V> {
    pub fn init() -> Self {
        Mailbox { inner: Vec::new() }
    }

    pub fn mmatch(
        &self,
        clauses: &Vec<Clause>,
        value_store: &SetMap<V, Value<V>>,
        module_env: &Env<V>,
        ast_helper: &AstHelper,
    ) -> Vec<(usize, Env<V>)> {
        let mut matched_msgs = Vec::new();
        for msg in &self.inner {
            let mats = Case::cmatch(
                clauses,
                &ValueAddressOrValue::Value(msg.clone()),
                value_store,
                module_env,
                ast_helper,
            );

            for opt in mats {
                if let Some(tup) = opt {
                    matched_msgs.push(tup);
                }
            }
        }

        matched_msgs
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Mailboxes<V: ValueAddress> {
    pub inner: BTreeMap<Pid, Mailbox<V>>,
}

impl<V: ValueAddress> Mailboxes<V> {
    pub fn init() -> Self {
        Mailboxes {
            inner: BTreeMap::from([(Pid::init(), Mailbox::init())]),
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
