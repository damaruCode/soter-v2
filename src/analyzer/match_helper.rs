use std::collections::BTreeMap;

use crate::{
    ast::{AstList, Clause, Index, Literal, TypedCore},
    state_space::{Closure, Env, Value, ValueAddress, VarName},
    util::{AstHelper, SetMap},
};

pub struct MatchHelper {}
impl MatchHelper {
    pub fn vmatch<V: ValueAddress>(
        clauses: &Vec<Clause>,
        v_addr: &V,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> BTreeMap<Value<V>, Vec<(usize, Vec<MatchSubstitution<V>>)>> {
        let mut matched_map = BTreeMap::new();
        for value in value_store.get(v_addr).unwrap() {
            let mut matches = Vec::new();
            for i in 0..clauses.len() {
                let substs = Self::cmatch(&clauses[i], value, value_store, ast_helper);

                if substs.len() > 0 {
                    if Self::gmatch(&*clauses[i].guard, value_store, ast_helper) {
                        matches.push((i, substs));
                    }
                }
            }
            matched_map.insert(value.clone(), matches);
        }

        matched_map
    }

    pub fn cmatch<V: ValueAddress>(
        clause: &Clause,
        value: &Value<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<MatchSubstitution<V>> {
        match value {
            Value::Closure(clo) => {
                match ast_helper.get(clo.prog_loc) {
                    TypedCore::Var(_) | TypedCore::Literal(_) => {
                        if clause.pats.inner.len() != 1 {
                            // there should only be one pattern
                            return Vec::new();
                        }

                        Self::amatch(&clause.pats.inner[0], value, value_store, ast_helper)
                    }
                    TypedCore::AstList(al) => {
                        Self::lmatch(clause, al, &clo.env, value_store, ast_helper)
                    }
                    TypedCore::Tuple(tup) => {
                        Self::lmatch(clause, &tup.es, &clo.env, value_store, ast_helper)
                    }
                    _ => Vec::new(),
                }
            }
            Value::Pid(_pid) => {
                if clause.pats.inner.len() != 1 {
                    // there should only be one pattern
                    return Vec::new();
                }

                Self::amatch(&clause.pats.inner[0], value, value_store, ast_helper)
            }
        }
    }

    fn lmatch<V: ValueAddress>(
        clause: &Clause,
        value: &AstList<TypedCore>,
        env: &Env<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<MatchSubstitution<V>> {
        let patterns = if clause.pats.inner.len() == 1 {
            if let TypedCore::Tuple(tup) = &clause.pats.inner[0] {
                &tup.es
            } else {
                &clause.pats
            }
        } else {
            &clause.pats
        };

        if value.inner.len() != patterns.inner.len() {
            return Vec::new();
        }

        let mut overall_substs = Vec::new();
        for i in 0..value.inner.len() {
            let substs_i = &Self::amatch(
                &patterns.inner[i],
                &Value::Closure(Closure {
                    prog_loc: value.inner[i].get_index().unwrap(),
                    env: env.clone(),
                }),
                value_store,
                ast_helper,
            );

            let mut overall_subst_i = MatchSubstitution::new();
            for subst in substs_i {
                overall_subst_i = overall_subst_i.join_with(subst);
            }
            if overall_subst_i.inner.len() != 0 {
                overall_substs.push(overall_subst_i);
            }
        }
        overall_substs
    }

    fn amatch<V: ValueAddress>(
        pattern: &TypedCore,
        value: &Value<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<MatchSubstitution<V>> {
        match pattern {
            TypedCore::Var(v) => {
                let mut new_subst = MatchSubstitution::new();
                new_subst.inner.insert(VarName::from(v), value.clone());
                Vec::from([new_subst])
            }
            TypedCore::Literal(pattern_l) => match value {
                Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                    TypedCore::Literal(msg_l) => {
                        if Self::literal_match(msg_l, pattern_l) {
                            Vec::from([MatchSubstitution::new()])
                        } else {
                            Vec::new()
                        }
                    }
                    _ => Vec::new(),
                },
                _ => Vec::new(),
            },
            _ => match value {
                Value::Closure(clo) => match &ast_helper.get(clo.prog_loc) {
                    TypedCore::Var(v) => {
                        let values = value_store
                            .get(clo.env.inner.get(&VarName::from(v)).unwrap())
                            .unwrap();

                        let mut new_substs = Vec::new();
                        for value in values {
                            new_substs.append(&mut Self::amatch(
                                &pattern,
                                value,
                                value_store,
                                ast_helper,
                            ));
                        }
                        new_substs
                    }
                    TypedCore::Literal(val_l) => match &pattern {
                        TypedCore::Literal(pattern_l) => {
                            if let TypedCore::String(val_s) = &*val_l.val {
                                if let TypedCore::String(pattern_s) = &*pattern_l.val {
                                    if val_s.inner == pattern_s.inner {
                                        Vec::from([MatchSubstitution::new()])
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

    fn literal_match(value_lit: &Literal, pattern_lit: &Literal) -> bool {
        if let TypedCore::String(s1) = &*pattern_lit.val {
            if let TypedCore::String(s2) = &*value_lit.val {
                if s1.inner == s2.inner {
                    return true;
                }
            }
        }

        return false;
    }

    pub fn gmatch<V: ValueAddress>(
        typed_core: &TypedCore,
        _value_store: &SetMap<V, Value<V>>,
        _ast_helper: &AstHelper,
    ) -> bool {
        match typed_core {
            TypedCore::Literal(l) => match *l.val.clone() {
                TypedCore::String(s) => {
                    if s.inner.as_str() == "true" {
                        true
                    } else {
                        todo!("{:#?}", typed_core)
                    }
                }
                TypedCore::Bool(b) => b.inner,
                _ => todo!("{:#?}", typed_core),
            },
            _ => todo!("{:#?}", typed_core),
        }
    }
}

#[derive(Debug)]
pub struct MatchSubstitution<V: ValueAddress> {
    pub inner: BTreeMap<VarName, Value<V>>,
}
impl<V: ValueAddress> MatchSubstitution<V> {
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
