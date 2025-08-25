use crate::{
    ast::{AstList, Clause, Index, Literal, TypedCore},
    util::{AstHelper, SetMap},
};

use super::{Closure, Env, Pid, Value, ValueAddress, VarName};
use std::{collections::BTreeMap, fmt::Display};

/// Mailbox := P(Value),
/// in this case this represents the AbsMailbox_set abstraction
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Mailbox<V: ValueAddress> {
    pub inner: Vec<V>,
}

impl<V: ValueAddress> Mailbox<V> {
    pub fn init() -> Self {
        Mailbox { inner: Vec::new() }
    }

    fn literal_match(msg_lit: &Literal, pattern_lit: &Literal) -> bool {
        if let TypedCore::String(s1) = &*pattern_lit.val {
            if let TypedCore::String(s2) = &*msg_lit.val {
                if s1.inner == s2.inner {
                    return true;
                }
            }
        }

        return false;
    }

    fn amatch<W: ValueAddress>(
        pattern: &TypedCore,
        msg_v_addr: &W,
        msg: &Value<W>,
        value_store: &SetMap<W, Value<W>>,
        ast_helper: &AstHelper,
    ) -> Vec<MailboxSubstitution<W>> {
        match pattern {
            TypedCore::Var(v) => {
                let mut new_subst = MailboxSubstitution::new();
                new_subst.inner.insert(VarName::from(v), msg_v_addr.clone());
                Vec::from([new_subst])
            }
            TypedCore::Literal(pattern_l) => match msg {
                Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                    TypedCore::Literal(msg_l) => {
                        if Self::literal_match(msg_l, pattern_l) {
                            Vec::from([MailboxSubstitution::new()])
                        } else {
                            Vec::new()
                        }
                    }
                    _ => Vec::new(),
                },
                _ => Vec::new(),
            },
            _ => match msg {
                Value::Closure(clo) => match &ast_helper.get(clo.prog_loc) {
                    TypedCore::Var(v) => {
                        let v_addr = clo.env.inner.get(&VarName::from(v)).unwrap();

                        let mut new_substs = Vec::new();
                        for value in value_store.get(v_addr).unwrap() {
                            new_substs.append(&mut Self::amatch(
                                &pattern,
                                v_addr,
                                value,
                                value_store,
                                ast_helper,
                            ));
                        }
                        new_substs
                    }
                    TypedCore::Literal(msg_l) => match &pattern {
                        TypedCore::Literal(pattern_l) => {
                            if let TypedCore::String(msg_s) = &*msg_l.val {
                                if let TypedCore::String(pattern_s) = &*pattern_l.val {
                                    if msg_s.inner == pattern_s.inner {
                                        Vec::from([MailboxSubstitution::new()])
                                    } else {
                                        Vec::new()
                                    }
                                } else {
                                    return Vec::new();
                                }
                            } else {
                                return Vec::new();
                            }
                        }
                        _ => Vec::new(),
                    },
                    _ => Vec::new(),
                },
                Value::Pid(_) => Vec::new(), // unmatchable if the pattern is not a var
            },
        }
    }

    fn lmatch<W: ValueAddress>(
        clause: &Clause,
        msg_v_addr: &W,
        msg: &AstList<TypedCore>,
        env: &Env<W>,
        value_store: &SetMap<W, Value<W>>,
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

        let mut overall_substs = Vec::new();
        for i in 0..msg.inner.len() {
            let substs_i = &Self::amatch(
                &patterns.inner[i],
                msg_v_addr,
                &Value::Closure(Closure {
                    prog_loc: msg.inner[i].get_index().unwrap(),
                    env: env.clone(),
                }),
                value_store,
                ast_helper,
            );

            let mut overall_subst_i = MailboxSubstitution::new();
            for subst in substs_i {
                overall_subst_i = overall_subst_i.join_with(subst);
            }
            if overall_subst_i.inner.len() != 0 {
                overall_substs.push(overall_subst_i);
            }
        }
        overall_substs
    }

    fn cmatch<W: ValueAddress>(
        clause: &Clause,
        msg_v_addr: &W,
        msg: &Value<W>,
        value_store: &SetMap<W, Value<W>>,
        ast_helper: &AstHelper,
    ) -> Vec<MailboxSubstitution<W>> {
        match msg {
            Value::Closure(clo) => {
                match ast_helper.get(clo.prog_loc) {
                    TypedCore::Var(_) | TypedCore::Literal(_) => {
                        if clause.pats.inner.len() != 1 {
                            // there should only be one pattern
                            return Vec::new();
                        }

                        Self::amatch(
                            &clause.pats.inner[0],
                            msg_v_addr,
                            msg,
                            value_store,
                            ast_helper,
                        )
                    }
                    TypedCore::AstList(al) => {
                        Self::lmatch(clause, msg_v_addr, al, &clo.env, value_store, ast_helper)
                    }
                    TypedCore::Tuple(tup) => Self::lmatch(
                        clause,
                        msg_v_addr,
                        &tup.es,
                        &clo.env,
                        value_store,
                        ast_helper,
                    ),
                    _ => Vec::new(),
                }
            }
            Value::Pid(_pid) => {
                if clause.pats.inner.len() != 1 {
                    // there should only be one pattern
                    return Vec::new();
                }

                Self::amatch(
                    &clause.pats.inner[0],
                    msg_v_addr,
                    msg,
                    value_store,
                    ast_helper,
                )
            }
        }
    }

    pub fn mmatch(
        &self,
        clauses: &Vec<Clause>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<(usize, Vec<MailboxSubstitution<V>>)> {
        let mut matched_msgs = Vec::new();

        for msg_v_addr in &self.inner {
            for msg in value_store.get(msg_v_addr).unwrap() {
                for i in 0..clauses.len() {
                    let substs =
                        Self::cmatch(&clauses[i], msg_v_addr, msg, value_store, ast_helper);
                    if substs.len() > 0 {
                        matched_msgs.push((i, substs));
                    }
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

    pub fn push(&mut self, pid: Pid, v_addr: V) {
        match self.inner.get_mut(&pid) {
            Some(mb) => {
                if !mb.inner.contains(&v_addr) {
                    mb.inner.push(v_addr);
                }
            }
            None => {
                let _ = self.inner.insert(
                    pid,
                    Mailbox {
                        inner: vec![v_addr],
                    },
                );
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
    pub inner: BTreeMap<VarName, V>,
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
