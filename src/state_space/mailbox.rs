use crate::{
    ast::{AstList, Clause, Index, TypedCore},
    util::{AstHelper, SetMap},
};

use super::{Closure, Env, Pid, Value, ValueAddress, VarName};
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

    fn amatch<W: ValueAddress>(
        pattern: &TypedCore,
        msg: &Value<W>,
        value_store: &SetMap<W, Value<W>>,
        proc_state_env: &Env<W>,
        ast_helper: &AstHelper,
    ) -> Vec<MailboxSubstitution<W>> {
        match pattern {
            TypedCore::Var(v) => {
                let mut new_subst = MailboxSubstitution::new();
                new_subst.inner.insert(VarName::from(v), msg.clone());
                Vec::from([new_subst])
            }
            TypedCore::Literal(pattern_l) => match msg {
                Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                    TypedCore::Literal(msg_l) => {
                        if let TypedCore::String(s1) = &*pattern_l.val {
                            if let TypedCore::String(s2) = &*msg_l.val {
                                if s1 == s2 {
                                    Vec::from([MailboxSubstitution::new()])
                                } else {
                                    Vec::new()
                                }
                            } else {
                                Vec::new()
                            }
                        } else {
                            Vec::new()
                        }
                    }
                    _ => Vec::new(),
                },
                _ => Vec::new(),
            },
            _ => match msg {
                Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                    TypedCore::Var(v) => {
                        let values = value_store
                            .get(clo.env.inner.get(&VarName::from(v)).unwrap())
                            .unwrap();

                        let mut new_substs = Vec::new();
                        for value in values {
                            new_substs.append(&mut Self::amatch(
                                pattern,
                                value,
                                value_store,
                                proc_state_env,
                                ast_helper,
                            ));
                        }
                        new_substs
                    }
                    _ => Vec::new(),
                },
                _ => Vec::new(),
            },
        }
    }

    fn lmatch<W: ValueAddress>(
        clause: &Clause,
        msg: &AstList<TypedCore>,
        env: &Env<W>,
        value_store: &SetMap<W, Value<W>>,
        proc_state_env: &Env<W>,
        ast_helper: &AstHelper,
    ) -> Vec<MailboxSubstitution<W>> {
        let patterns = if clause.pats.inner.len() == 1 {
            if let TypedCore::Tuple(tup) = &clause.pats.inner[0] {
                &tup.es
            } else {
                &clause.pats
            }
        } else {
            &clause.pats
        };

        if msg.inner.len() != patterns.inner.len() {
            return Vec::new();
        }

        let mut new_substs = Vec::new();
        for i in 0..msg.inner.len() {
            let substs = &Self::amatch(
                &patterns.inner[i],
                &Value::Closure(Closure {
                    prog_loc: msg.inner[i].get_index().unwrap(),
                    env: env.clone(),
                }),
                value_store,
                proc_state_env,
                ast_helper,
            );

            let mut new_subst = MailboxSubstitution::new();
            for subst in substs {
                new_subst = new_subst.join_with(subst);
            }
            if new_subst.inner.len() != 0 {
                new_substs.push(new_subst);
            }
        }
        new_substs
    }

    fn cmatch<W: ValueAddress>(
        clause: &Clause,
        msg: &Value<W>,
        value_store: &SetMap<W, Value<W>>,
        proc_state_env: &Env<W>,
        ast_helper: &AstHelper,
    ) -> Vec<MailboxSubstitution<W>> {
        match msg {
            Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                TypedCore::Var(_) => {
                    if clause.pats.inner.len() != 1 {
                        // there should only be one pattern
                        return Vec::new();
                    }

                    Self::amatch(
                        &clause.pats.inner[0],
                        msg,
                        value_store,
                        proc_state_env,
                        ast_helper,
                    )
                }
                TypedCore::AstList(al) => Self::lmatch(
                    clause,
                    al,
                    &clo.env,
                    value_store,
                    proc_state_env,
                    ast_helper,
                ),
                TypedCore::Tuple(tup) => Self::lmatch(
                    clause,
                    &tup.es,
                    &clo.env,
                    value_store,
                    proc_state_env,
                    ast_helper,
                ),
                _ => Vec::new(),
            },
            Value::Pid(_pid) => {
                if clause.pats.inner.len() != 1 {
                    return Vec::new();
                }

                Self::amatch(
                    &clause.pats.inner[0],
                    msg,
                    value_store,
                    proc_state_env,
                    ast_helper,
                )
            }
        }
    }

    pub fn mmatch(
        &self,
        clauses: &Vec<Clause>,
        value_store: &SetMap<V, Value<V>>,
        proc_state_env: &Env<V>,
        ast_helper: &AstHelper,
    ) -> Vec<(usize, MailboxSubstitution<V>)> {
        let mut matched_msgs = Vec::new();

        for msg in &self.inner {
            for i in 0..clauses.len() {
                let substs =
                    Self::cmatch(&clauses[i], msg, value_store, proc_state_env, ast_helper);
                for subst in substs {
                    matched_msgs.push((i, subst));
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

#[derive(Debug)]
pub struct MailboxSubstitution<V: ValueAddress> {
    pub inner: BTreeMap<VarName, Value<V>>,
}
impl<V: ValueAddress> MailboxSubstitution<V> {
    pub fn new() -> Self {
        Self {
            inner: BTreeMap::new(),
        }
    }

    pub fn join_with(&self, other_subst: &Self) -> Self {
        let mut new_subst = Self::new();
        for (var_name, value1) in &self.inner {
            match other_subst.inner.get(var_name) {
                Some(value2) => {
                    if value1 != value2 {
                        return Self::new();
                    }
                }
                None => {
                    new_subst.inner.insert(var_name.clone(), value1.clone());
                }
            }
        }

        for (var_name, value) in &other_subst.inner {
            if !self.inner.contains_key(var_name) {
                new_subst.inner.insert(var_name.clone(), value.clone());
            }
        }
        new_subst
    }
}
