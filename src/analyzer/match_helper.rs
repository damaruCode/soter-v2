use std::{collections::BTreeMap, fmt::Display, iter::zip};

use crate::{
    ast::{AstList, Clause, Cons, Index, Literal, Tuple, TypedCore, ValueAddressOrValue, Var},
    state_space::{Closure, Env, Value, ValueAddress},
    util::{AstHelper, SetMap},
};

pub struct MatchHelper {}
impl MatchHelper {
    /// Matches all clauses against the v_addr
    ///
    /// ## Arguments
    /// * `clauses` - all clauses to match on (from a `receive` or `case` expression)
    /// * `v_addr` - a ValueAddress respresenting all possible values for a variable
    /// * `value_store` - the current Value Store to lookup all values associated with the `v_addr`
    /// * `ast_helper` - the AstHelper to lookup relevant nodes in the abstract syntax tree
    ///
    /// ## Returns
    /// A BTreeMap of clause indexes that matched and the witnessing substitutions for each distinct
    /// value.
    ///
    /// ## Panics
    ///
    /// ## Errors
    ///
    pub fn cs_match_vaddr<V: ValueAddress>(
        clauses: &Vec<Clause>,
        v_addr: &V,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> BTreeMap<usize, Vec<MatchSubstitution<V>>> {
        let mut clause_matches = BTreeMap::new();

        for i in 0..clauses.len() {
            let substs = Self::p_match_vaddr(
                if clauses[i].pats.inner.len() == 1 {
                    (*clauses[i].pats.inner)[0].as_pattern()
                } else {
                    clauses[i].pats.as_pattern()
                },
                v_addr,
                value_store,
                ast_helper,
            );

            if substs.len() > 0 {
                if Self::match_guard(&*clauses[i].guard, value_store, ast_helper) {
                    clause_matches.insert(i, substs);
                }
            }
        }

        clause_matches
    }

    /// Matches all clauses against the argument value
    ///
    /// ## Arguments
    /// * `clauses` - all clauses to match on (from a `receive` or `case` expression)
    /// * `v_addr` - a ValueAddress respresenting all possible values for a variable
    /// * `value_store` - the current Value Store to lookup all values associated with the `v_addr`
    /// * `ast_helper` - the AstHelper to lookup relevant nodes in the abstract syntax tree
    ///
    /// ## Returns
    /// A BTreeMap of clause indexes that matched and the witnessing substitutions for each distinct
    /// value.
    ///
    /// ## Panics
    ///
    /// ## Errors
    ///
    pub fn cs_match_value<V: ValueAddress>(
        clauses: &Vec<Clause>,
        value: &Value<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> BTreeMap<usize, Vec<MatchSubstitution<V>>> {
        let mut clause_matches = BTreeMap::new();

        for i in 0..clauses.len() {
            let clause_pat = if clauses[i].pats.inner.len() == 1 {
                // if pats is unary, handle it as a single expression
                (*clauses[i].pats.inner)[0].as_pattern()
            } else {
                // otherwise handle it as a value list
                clauses[i].pats.as_pattern()
            };
            let substs = Self::p_match_value(&clause_pat, value, value_store, ast_helper);

            if substs.len() > 0 {
                if Self::match_guard(&*clauses[i].guard, value_store, ast_helper) {
                    clause_matches.insert(i, substs);
                }
            }
        }

        clause_matches
    }

    /// Matches a pattern against a single element of a compound value (the head/tail of
    /// a `Cons`, or an element of a `Tuple`/`Values`).
    ///
    /// When the element is itself a variable, it is resolved through its value address
    /// via [`Self::p_match_vaddr`] rather than being wrapped in a `Closure` whose
    /// `prog_loc` points at the `Var` node. Such variable-closures can become
    /// self-referential under address reuse (e.g. 0-CFA), which would make later
    /// resolution (e.g. `resolve_pid`) loop indefinitely. Resolving through the address
    /// instead binds pattern variables directly to the existing address.
    ///
    /// ## Arguments
    /// * `pattern` - the pattern to match this element against
    /// * `value_elem` - the AST node of the compound value's element
    /// * `env` - the environment captured by the compound value's closure
    /// * `value_store` - the current Value Store
    /// * `ast_helper` - the AstHelper to lookup relevant nodes in the abstract syntax tree
    fn match_element<V: ValueAddress>(
        pattern: PatternKind,
        value_elem: &TypedCore,
        env: &Env<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<MatchSubstitution<V>> {
        match value_elem {
            TypedCore::Var(v) => {
                let v_addr = env.inner.get(v.var_id.unwrap()).unwrap();
                Self::p_match_vaddr(pattern, v_addr, value_store, ast_helper)
            }
            _ => {
                let value = Value::Closure(Closure {
                    prog_loc: value_elem.get_index().unwrap(),
                    env: env.clone(),
                });
                Self::p_match_value(&pattern, &value, value_store, ast_helper)
            }
        }
    }

    /// Matches a pattern to all values behind a value address
    ///
    /// ## Arguments
    ///
    /// ## Returns
    ///
    /// ## Panics
    ///
    /// ## Errors
    ///
    fn p_match_vaddr<V: ValueAddress>(
        pattern: PatternKind,
        v_addr: &V,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<MatchSubstitution<V>> {
        match pattern {
            PatternKind::Var(pv) => {
                let mut subst = MatchSubstitution::new();

                subst.inner.insert(
                    pv.var_id.unwrap().clone(),
                    ValueAddressOrValue::ValueAddress(v_addr.clone()),
                );
                Vec::from([subst])
            }
            _ => {
                let mut substs = Vec::new();
                for value in value_store.get(v_addr).unwrap() {
                    let mut p_match_substs =
                        Self::p_match_value(&pattern, value, value_store, ast_helper);
                    substs.append(&mut p_match_substs)
                }
                substs
            }
        }
    }

    /// Matches a pattern to a value
    ///
    /// ## Arguments
    ///
    /// ## Returns
    ///
    /// ## Panics
    ///
    /// ## Errors
    ///
    fn p_match_value<V: ValueAddress>(
        pattern: &PatternKind,
        value: &Value<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> Vec<MatchSubstitution<V>> {
        match pattern {
            PatternKind::Cons(pc) => match value {
                Value::Closure(v_clo) => match ast_helper.get(v_clo.prog_loc) {
                    TypedCore::Cons(vc) => {
                        if pc.iter().count() != vc.iter().count() {
                            return Vec::new();
                        }

                        let pairs = zip(pc.iter().map(|tc| tc.as_pattern()), vc);

                        let mut substs: Option<Vec<MatchSubstitution<V>>> = None;
                        for (p, v) in pairs {
                            let res =
                                Self::match_element(p, v, &v_clo.env, value_store, ast_helper);

                            if res.is_empty() {
                                // no matching value => done
                                return Vec::new();
                            }

                            // product with all previous substs
                            substs = if let Some(inner) = substs {
                                // for any other iteration
                                Some(
                                    inner
                                        .iter()
                                        .flat_map(|subst| subst.mult_with(&res))
                                        .collect(),
                                )
                            } else {
                                // for first iteration
                                Some(res)
                            };
                        }

                        substs.unwrap_or(Vec::new())
                    }
                    _ => Vec::new(), // can't match cons to anything other than cons
                },
                Value::Pid(_) => Vec::new(), // can't match cons to pid
            },
            PatternKind::Tuple(pt) => match value {
                Value::Closure(v_clo) => match ast_helper.get(v_clo.prog_loc) {
                    TypedCore::Tuple(vt) => {
                        if pt.es.inner.len() != vt.es.inner.len() {
                            return Vec::new();
                        }

                        let pairs = zip(
                            pt.es.inner.iter().map(|tc| tc.as_pattern()),
                            vt.es.inner.clone(),
                        );

                        let mut substs: Option<Vec<MatchSubstitution<V>>> = None;
                        for (p, v) in pairs {
                            let res =
                                Self::match_element(p, &v, &v_clo.env, value_store, ast_helper);

                            if res.is_empty() {
                                // no matching value => done
                                return Vec::new();
                            }

                            // product with all previous substs
                            substs = if let Some(inner) = substs {
                                // for any other iteration
                                Some(
                                    inner
                                        .iter()
                                        .flat_map(|subst| subst.mult_with(&res))
                                        .collect(),
                                )
                            } else {
                                // for first iteration
                                Some(res)
                            };
                        }

                        substs.unwrap_or(Vec::new())
                    }
                    _ => Vec::new(), // can't match tuple to anything other than tuple
                },
                Value::Pid(_) => Vec::new(), // can"t match tuple to pid
            },
            PatternKind::Values(pal) => match value {
                Value::Closure(v_clo) => match ast_helper.get(v_clo.prog_loc) {
                    TypedCore::Values(vv) => {
                        if pal.inner.len() != vv.es.inner.len() {
                            return Vec::new();
                        }

                        let pairs = zip(
                            pal.inner.iter().map(|tc| tc.as_pattern()),
                            vv.es.inner.clone(),
                        );

                        let mut substs: Option<Vec<MatchSubstitution<V>>> = None;
                        for (p, v) in pairs {
                            let res =
                                Self::match_element(p, &v, &v_clo.env, value_store, ast_helper);

                            if res.is_empty() {
                                // no matching value => done
                                return Vec::new();
                            }

                            // product with all previous substs
                            substs = if let Some(inner) = substs {
                                // for any other iteration
                                Some(
                                    inner
                                        .iter()
                                        .flat_map(|subst| subst.mult_with(&res))
                                        .collect(),
                                )
                            } else {
                                // for first iteration
                                Some(res)
                            };
                        }

                        substs.unwrap_or(Vec::new())
                    }
                    _ => Vec::new(), // can't match values to anything other than values
                },
                Value::Pid(_) => Vec::new(), // can't match values to pid
            },
            PatternKind::Literal(pl) => match value {
                Value::Closure(v_clo) => match ast_helper.get(v_clo.prog_loc) {
                    TypedCore::Literal(v_lit) => {
                        if Self::literal_cmp(v_lit, pl) {
                            Vec::from([MatchSubstitution::new()])
                        } else {
                            Vec::new()
                        }
                    }
                    _ => Vec::new(), // can't match literal to anything other than a
                                     // literal
                },
                Value::Pid(_) => Vec::new(), // can't match literal to pid
            },
            PatternKind::Var(pv) => {
                let mut subst = MatchSubstitution::new();

                subst.inner.insert(
                    pv.var_id.unwrap().clone(),
                    ValueAddressOrValue::Value(value.clone()),
                );
                Vec::from([subst])
            }
        }
    }

    fn literal_val_eq(a: &TypedCore, b: &TypedCore) -> bool {
        match (a, b) {
            (TypedCore::Null(_), TypedCore::Null(_)) => true,
            (TypedCore::Bool(a), TypedCore::Bool(b)) => a.inner == b.inner,
            (TypedCore::Number(a), TypedCore::Number(b)) => a.inner == b.inner,
            (TypedCore::String(a), TypedCore::String(b)) => a.inner == b.inner,
            (TypedCore::AstList(a), TypedCore::AstList(b)) => {
                a.inner.len() == b.inner.len()
                    && a.inner
                        .iter()
                        .zip(&b.inner)
                        .all(|(x, y)| Self::literal_val_eq(x, y))
            }
            _ => false,
        }
    }

    fn literal_cmp(a: &Literal, b: &Literal) -> bool {
        Self::literal_val_eq(&a.val, &b.val)
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

#[derive(Debug, PartialEq, Eq)]
pub struct MatchSubstitution<V: ValueAddress> {
    pub inner: BTreeMap<usize, ValueAddressOrValue<V>>,
}
impl<V: ValueAddress> MatchSubstitution<V> {
    pub fn new() -> Self {
        Self {
            inner: BTreeMap::new(),
        }
    }

    pub fn join_with(&self, other_subst: &Self) -> Option<Self> {
        let mut new_subst = Self::new();
        for (var_name, value1) in &self.inner {
            match other_subst.inner.get(var_name) {
                Some(value2) => {
                    if value1 != value2 {
                        // incompatible; no viable join
                        return None;
                    }
                    // compatible; carry over
                    new_subst.inner.insert(var_name.clone(), value1.clone());
                }
                None => {
                    // only in self; carry over
                    new_subst.inner.insert(var_name.clone(), value1.clone());
                }
            }
        }
        for (var_name, value) in &other_subst.inner {
            if !self.inner.contains_key(var_name) {
                // only in other; carry over
                new_subst.inner.insert(var_name.clone(), value.clone());
            }
        }
        Some(new_subst)
    }

    pub fn mult_with(&self, other_substs: &Vec<Self>) -> Vec<Self> {
        // join self with all other_substs
        other_substs
            .iter()
            .filter_map(|other_subst| self.join_with(other_subst))
            .collect()
    }
}

// TODO move the below code to a suitable location
#[derive(Debug)]
pub enum PatternKind {
    Var(Var),
    Literal(Literal), // also includes the literal empty list
    Cons(Cons),
    Tuple(Tuple),
    Values(AstList<TypedCore>),
}
impl Display for PatternKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            PatternKind::Var(v) => v.fmt(f),
            PatternKind::Cons(c) => c.fmt(f),
            PatternKind::Literal(l) => l.fmt(f),
            PatternKind::Tuple(t) => t.fmt(f),
            PatternKind::Values(al) => al.fmt(f),
        }
    }
}

impl TypedCore {
    // TODO could probably also be the From<> trait
    pub fn as_pattern(&self) -> PatternKind {
        match self {
            TypedCore::Var(v) => PatternKind::Var(v.clone()),
            TypedCore::Literal(l) => PatternKind::Literal(l.clone()),
            TypedCore::Cons(c) => PatternKind::Cons(c.clone()),
            TypedCore::Tuple(t) => PatternKind::Tuple(t.clone()),
            TypedCore::AstList(al) => PatternKind::Values(al.clone()),
            _ => panic!("Invalid typed core for pattern. Should already have been handled by the erlang compiler.")
        }
    }
}

impl AstList<TypedCore> {
    pub fn as_pattern(&self) -> PatternKind {
        PatternKind::Values(self.clone())
    }
}
