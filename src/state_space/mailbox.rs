use crate::{
    analyzer::{MatchHelper, MatchSubstitution},
    ast::Clause,
    util::{AstHelper, SetMap},
};

use super::{Pid, Value, ValueAddress};
use std::{collections::BTreeMap, fmt::Display};

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
        ast_helper: &AstHelper,
    ) -> Vec<(usize, Vec<MatchSubstitution<V>>)> {
        let mut matched_msgs = Vec::new();

        for msg in &self.inner {
            for i in 0..clauses.len() {
                let substs = MatchHelper::cmatch(&clauses[i], msg, value_store, ast_helper);
                if substs.len() > 0 {
                    matched_msgs.push((i, substs));
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

impl<V: ValueAddress> Display for Mailbox<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut strs = Vec::new();
        for msg in &self.inner {
            strs.push(format!("{}", msg));
        }
        write!(f, "{}", strs.join(","))
    }
}
