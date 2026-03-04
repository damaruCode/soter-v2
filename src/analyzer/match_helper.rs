use std::collections::BTreeMap;

use crate::{
    ast::{AstList, Clause, Index, Literal, MaybeIndex, TypedCore},
    state_space::{Closure, Env, Value, ValueAddress},
    util::{AstHelper, SetMap},
};

pub struct MatchHelper {}
impl MatchHelper {
    /// Matches all values bound to a VAddr in the Store to a set of clauses and returns all
    /// matching substitutions.
    ///
    /// ## Arguments
    /// * `clauses` - all clauses to match on (from a `receive` or `case` expression)
    /// * `v_addr` - a ValueAddress respresenting all possible values for a variable
    /// * `value_store` - the current Value Store to lookup all values associated with the `v_addr`
    /// * `ast_helper` - the AstHelper to lookup relevant nodes in the abstract syntax tree
    ///
    /// ## Returns
    /// A BTreeMap mapping a choice of value to a vector of pairs `(clause_id, substitutions)`, where
    /// the `clause_id` is the index of the matching clause and `substitutions` is equivalent to an
    /// environment mapping variables (from any patterns in the clause) to their value (from the
    /// chosen value).
    ///
    /// ## Panics
    /// This function does no panic.
    ///
    /// ## Errors
    /// If a certain value does not match any clause, it will not be in the returned BTreeMap
    pub fn cmatch_values<V: ValueAddress>(
        clauses: &Vec<Clause>,
        v_addr: &V,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> BTreeMap<Value<V>, Vec<(usize, Vec<MatchSubstitution<V>>)>> {
        let mut matched_map = BTreeMap::new();
        for value in value_store.get(v_addr).unwrap() {
            let mut matches = Vec::new();
            for i in 0..clauses.len() {
                let substs = Self::cmatch_value(&clauses[i], value, value_store, ast_helper);

                if substs.len() > 0 {
                    if Self::match_guard(&*clauses[i].guard, value_store, ast_helper) {
                        matches.push((i, substs));
                    }
                }
            }
            matched_map.insert(value.clone(), matches);
        }

        matched_map
    }

    /// Matches a value to a clause
    ///
    /// ## Arguments
    /// * `clause` - a singular clause (from a `receive` or `case` expression)
    /// * `value` - a singular choice of value (chosen from a vector of possible values associated
    /// with a variable)
    /// * `value_store` - the current Value Store to lookup any additional values associated with
    /// variables in `value`
    /// * `ast_helper` - the AstHelper to lookup relevant nodes in the abstract syntax tree
    ///
    /// ## Returns
    /// A vector of (possibly empty) substitutions representing a successful match and any
    /// additional bindings for the body of the clause.
    ///
    /// ## Panics
    /// The function does not panic.
    ///
    /// ## Errors
    /// If the `clause` does not match the `value` the function returns an empty vector
    /// representing no matches.
    ///
    pub fn cmatch_value<V: ValueAddress>(
        clause: &Clause,
        value: &Value<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<MatchSubstitution<V>> {
        // TODO why a vector of substitutions?
        match value {
            Value::Closure(clo) => {
                match ast_helper.get(clo.prog_loc) {
                    TypedCore::Var(_) | TypedCore::Literal(_) => {
                        if clause.pats.inner.len() != 1 {
                            // there should only be one pattern
                            return Vec::new();
                        }

                        Self::pmatch_value(&clause.pats.inner[0], value, value_store, ast_helper)
                    }
                    TypedCore::AstList(al) => {
                        Self::cmatch_list(clause, al, &clo.env, value_store, ast_helper)
                    }
                    TypedCore::Tuple(tup) => {
                        Self::cmatch_list(clause, &tup.es, &clo.env, value_store, ast_helper)
                    }
                    _ => Vec::new(),
                }
            }
            Value::Pid(_pid) => {
                if clause.pats.inner.len() != 1 {
                    // there should only be one pattern
                    return Vec::new();
                }

                Self::pmatch_value(&clause.pats.inner[0], value, value_store, ast_helper)
            }
        }
    }

    /// Matches an AstList against a clause
    ///
    /// ## Arguments
    /// * `clause` - a singular clause (from a `receive` or `case` expression)
    /// * `value` - an erlang list // NOTE superficially because AstList is only a JSON list, that
    /// does not neccessarily have to correspond to an erlang list --- fix is on the way
    /// * `env` - ???
    /// * `value_store` - the current Value Store to lookup any additional values associated with
    /// variables in `value`
    /// * `ast_helper` - the AstHelper to lookup relevant nodes in the abstract syntax tree
    ///
    /// ## Returns
    /// A vector of (possibly empty) substitutions representing a successful match and any
    /// additional bindings for the body of the clause.
    ///
    /// ## Panics
    /// The function does not panic.
    ///
    /// ## Errors
    /// If the `clause` does not match the `value` the function returns an empty vector
    /// representing no matches.
    ///
    fn cmatch_list<V: ValueAddress>(
        clause: &Clause,
        value: &AstList<TypedCore>,
        env: &Env<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<MatchSubstitution<V>> {
        //TODO dont flatten the structure
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
            let substs_i = &Self::pmatch_value(
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

    /// Matches a value against a pattern
    ///
    /// ## Arguments
    /// * `pattern` - a singular pattern (from a clause of `case` or `receive`)
    /// * `value` - a value
    /// * `value_store` - the current Value Store to lookup any additional values associated with
    /// variables in `value`
    /// * `ast_helper` - the AstHelper to lookup relevant nodes in the abstract syntax tree
    ///
    /// ## Returns
    /// A vector of (possibly empty) substitutions representing a successful match and any
    /// additional bindings for the body of the clause.
    ///
    /// ## Panics
    /// The function does not panic.
    ///
    /// ## Errors
    /// If the `clause` does not match the `value` the function returns an empty vector
    /// representing no matches.
    ///
    fn pmatch_value<V: ValueAddress>(
        pattern: &TypedCore,
        value: &Value<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<MatchSubstitution<V>> {
        match pattern {
            TypedCore::Var(v) => {
                let mut new_subst = MatchSubstitution::new();
                new_subst.inner.insert(v.var_id.clone(), value.clone());
                Vec::from([new_subst])
            }
            TypedCore::Literal(pattern_l) => match value {
                Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
                    TypedCore::Literal(msg_l) => {
                        if Self::literal_cmp(msg_l, pattern_l) {
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
                    TypedCore::Var(v) => match &v.var_id {
                        MaybeIndex::Some(var_id) => {
                            let values =
                                value_store.get(clo.env.inner.get(var_id).unwrap()).unwrap();

                            let mut new_substs = Vec::new();
                            for value in values {
                                new_substs.append(&mut Self::pmatch_value(
                                    &pattern,
                                    value,
                                    value_store,
                                    ast_helper,
                                ));
                            }
                            new_substs
                        }
                        MaybeIndex::None => Vec::new(),
                    },
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

    fn literal_cmp(value_lit: &Literal, pattern_lit: &Literal) -> bool {
        if let TypedCore::String(s1) = &*pattern_lit.val {
            if let TypedCore::String(s2) = &*value_lit.val {
                if s1.inner == s2.inner {
                    return true;
                }
            }
        }

        return false;
    }

    /// Checks a guard of a clause
    /// **NOTE** right now only a literal `true`
    ///
    /// ## Arguments
    /// * `typed_core` - a node of the abstract syntax tree
    /// * `_value_store` - the current Value Store to lookup any additional values associated with
    /// variables in `typed_core`
    /// * `_ast_helper` - the AstHelper to lookup relevant nodes in the abstract syntax tree
    ///
    /// ## Returns
    ///
    /// ## Panics
    /// Panics if `typed_core` is not a literal string "true"
    ///
    /// ## Errors
    /// None
    ///
    pub fn match_guard<V: ValueAddress>(
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
    pub inner: BTreeMap<MaybeIndex, Value<V>>,
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
