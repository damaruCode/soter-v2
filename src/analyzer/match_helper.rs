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
        clauses: &[Clause],
        v_addr: &V,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> BTreeMap<usize, Vec<MatchSubstitution<V>>> {
        let mut clause_matches = BTreeMap::new();

        for (i, clause) in clauses.iter().enumerate() {
            let substs = Self::p_match_vaddr(
                if clause.pats.inner.len() == 1 {
                    (*clause.pats.inner)[0].as_pattern()
                } else {
                    clause.pats.as_pattern()
                },
                v_addr,
                value_store,
                ast_helper,
            );

            if !substs.is_empty() && Self::match_guard(&clauses[i].guard, value_store, ast_helper) {
                clause_matches.insert(i, substs);
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
        clauses: &[Clause],
        value: &Value<V>,
        value_store: &SetMap<V, Value<V>>,
        ast_helper: &AstHelper,
    ) -> BTreeMap<usize, Vec<MatchSubstitution<V>>> {
        let mut clause_matches = BTreeMap::new();

        for (i, clause) in clauses.iter().enumerate() {
            let clause_pat = if clause.pats.inner.len() == 1 {
                // if pats is unary, handle it as a single expression
                (*clause.pats.inner)[0].as_pattern()
            } else {
                // otherwise handle it as a value list
                clause.pats.as_pattern()
            };
            let substs = Self::p_match_value(&clause_pat, value, value_store, ast_helper);

            if !substs.is_empty() && Self::match_guard(&clause.guard, value_store, ast_helper) {
                clause_matches.insert(i, substs);
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
                    *pv.var_id.unwrap(),
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

                        substs.unwrap_or_default()
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

                        substs.unwrap_or_default()
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

                        substs.unwrap_or_default()
                    }
                    _ => Vec::new(), // can't match values to anything other than values
                },
                Value::Pid(_) => Vec::new(), // can't match values to pid
            },
            PatternKind::Literal(pl) => match value {
                Value::Closure(v_clo) => match ast_helper.get(v_clo.prog_loc) {
                    TypedCore::Literal(v_lit) => {
                        println!("pat: {pl}, val: {v_lit}");
                        if Self::literal_cmp(v_lit, pl) {
                            println!("passed");
                            Vec::from([MatchSubstitution::new()])
                        } else {
                            println!("did not pass");
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
                    *pv.var_id.unwrap(),
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
    /// * `_value_store` - the current Value Store to lookup any additional values associated with variables in `typed_core`
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
impl<V: ValueAddress> Default for MatchSubstitution<V> {
    fn default() -> Self {
        Self::new()
    }
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
                    new_subst.inner.insert(*var_name, value1.clone());
                }
                None => {
                    // only in self; carry over
                    new_subst.inner.insert(*var_name, value1.clone());
                }
            }
        }
        for (var_name, value) in &other_subst.inner {
            if !self.inner.contains_key(var_name) {
                // only in other; carry over
                new_subst.inner.insert(*var_name, value.clone());
            }
        }
        Some(new_subst)
    }

    pub fn mult_with(&self, other_substs: &[Self]) -> Vec<Self> {
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

#[cfg(test)]
mod tests {
    use super::{MatchHelper, MatchSubstitution, PatternKind};
    use crate::abstraction::standard::VAddr;
    use crate::ast::{
        AstList, ErlBool, ErlNull, ErlNumber, ErlString, Literal, MaybeIndex, TypedCore,
        ValueAddressOrValue, Var,
    };
    use crate::state_space::{Pid, Time, Value};
    use crate::util::{AstHelper, SetMap};
    use serde_json::Number;

    // ---------------
    // construction helpers
    // ---------------

    fn null() -> TypedCore {
        TypedCore::Null(ErlNull::new())
    }

    fn boolean(b: bool) -> TypedCore {
        TypedCore::Bool(ErlBool::new(b))
    }

    fn number(n: i64) -> TypedCore {
        TypedCore::Number(ErlNumber::new(Number::from(n)))
    }

    fn string(s: &str) -> TypedCore {
        TypedCore::String(ErlString::new(s.to_string()))
    }

    fn ast_list(elems: Vec<TypedCore>) -> TypedCore {
        let mut list = AstList::new();
        for elem in elems {
            list.inner.push(elem);
        }
        TypedCore::AstList(list)
    }

    fn literal(val: TypedCore) -> Literal {
        let mut lit = Literal::new();
        lit.val = Box::new(val);
        lit
    }

    fn empty_store() -> SetMap<VAddr, Value<VAddr>> {
        SetMap::new()
    }

    // ---------------
    // literal_val_eq
    // ---------------

    #[test]
    fn literal_val_eq_null() {
        assert!(MatchHelper::literal_val_eq(&null(), &null()));
    }

    #[test]
    fn literal_val_eq_bool() {
        assert!(MatchHelper::literal_val_eq(&boolean(true), &boolean(true)));
        assert!(!MatchHelper::literal_val_eq(
            &boolean(true),
            &boolean(false)
        ));
    }

    #[test]
    fn literal_val_eq_number() {
        assert!(MatchHelper::literal_val_eq(&number(1), &number(1)));
        assert!(!MatchHelper::literal_val_eq(&number(1), &number(2)));
    }

    #[test]
    fn literal_val_eq_string() {
        assert!(MatchHelper::literal_val_eq(&string("a"), &string("a")));
        assert!(!MatchHelper::literal_val_eq(&string("a"), &string("b")));
    }

    #[test]
    fn literal_val_eq_nested_list() {
        // Equal lists compare element-wise, recursively.
        let a = ast_list(vec![number(1), string("x")]);
        let b = ast_list(vec![number(1), string("x")]);
        assert!(MatchHelper::literal_val_eq(&a, &b));

        let c = ast_list(vec![number(1), string("y")]);
        assert!(!MatchHelper::literal_val_eq(&a, &c));
    }

    #[test]
    fn literal_val_eq_list_length_differs() {
        let a = ast_list(vec![number(1)]);
        let b = ast_list(vec![number(1), number(2)]);
        assert!(!MatchHelper::literal_val_eq(&a, &b));
    }

    #[test]
    fn literal_val_eq_type_mismatch() {
        assert!(!MatchHelper::literal_val_eq(&string("1"), &number(1)));
        assert!(!MatchHelper::literal_val_eq(&null(), &boolean(true)));
    }

    // ---------------
    // literal_cmp
    // ---------------

    #[test]
    fn literal_cmp_delegates_to_val_eq() {
        assert!(MatchHelper::literal_cmp(
            &literal(number(1)),
            &literal(number(1))
        ));
        assert!(!MatchHelper::literal_cmp(
            &literal(number(1)),
            &literal(number(2))
        ));
    }

    // ---------------
    // p_match_value: arms reachable without an AST lookup
    // ---------------

    fn var_pattern(id: usize) -> PatternKind {
        let mut v = Var::new();
        v.var_id = MaybeIndex::Some(id);
        PatternKind::Var(v)
    }

    #[test]
    fn p_match_value_var_binds_value() {
        // A variable pattern matches any value, binding it directly.
        let store = empty_store();
        let ast_helper = AstHelper::new();
        let value = Value::Pid(Pid::init());

        let substs = MatchHelper::p_match_value(&var_pattern(7), &value, &store, &ast_helper);

        assert_eq!(substs.len(), 1);
        assert_eq!(
            substs[0].inner.get(&7),
            Some(&ValueAddressOrValue::Value(value))
        );
    }

    #[test]
    fn p_match_value_literal_vs_pid_no_match() {
        // A literal pattern can never match a pid.
        let store = empty_store();
        let ast_helper = AstHelper::new();
        let pattern = PatternKind::Literal(literal(string("a")));

        let substs =
            MatchHelper::p_match_value(&pattern, &Value::Pid(Pid::init()), &store, &ast_helper);

        assert!(substs.is_empty());
    }

    // ---------------
    // match_guard
    // ---------------

    #[test]
    fn match_guard_bool_true() {
        let store = empty_store();
        let ast_helper = AstHelper::new();
        assert!(MatchHelper::match_guard(
            &TypedCore::Literal(literal(boolean(true))),
            &store,
            &ast_helper
        ));
    }

    #[test]
    fn match_guard_bool_false() {
        let store = empty_store();
        let ast_helper = AstHelper::new();
        assert!(!MatchHelper::match_guard(
            &TypedCore::Literal(literal(boolean(false))),
            &store,
            &ast_helper
        ));
    }

    // ---------------
    // MatchSubstitution: join_with / mult_with
    // ---------------

    fn val(pid_loc: usize) -> ValueAddressOrValue<VAddr> {
        ValueAddressOrValue::Value(Value::Pid(Pid {
            prog_loc: pid_loc,
            time: Time::init(),
        }))
    }

    fn subst_of(pairs: Vec<(usize, ValueAddressOrValue<VAddr>)>) -> MatchSubstitution<VAddr> {
        let mut subst = MatchSubstitution::new();
        for (var_id, value) in pairs {
            subst.inner.insert(var_id, value);
        }
        subst
    }

    #[test]
    fn match_substitution_join_disjoint() {
        let a = subst_of(vec![(0, val(0))]);
        let b = subst_of(vec![(1, val(1))]);
        let joined = a.join_with(&b).expect("disjoint substitutions should join");
        assert_eq!(joined.inner.get(&0), Some(&val(0)));
        assert_eq!(joined.inner.get(&1), Some(&val(1)));
        assert_eq!(joined.inner.len(), 2);
    }

    #[test]
    fn match_substitution_join_compatible() {
        // Overlapping key `0` carries the same value in both, so the join succeeds.
        let a = subst_of(vec![(0, val(0)), (1, val(1))]);
        let b = subst_of(vec![(0, val(0)), (2, val(2))]);
        let joined = a
            .join_with(&b)
            .expect("compatible substitutions should join");
        assert_eq!(joined.inner.len(), 3);
    }

    #[test]
    fn match_substitution_join_incompatible() {
        // Key `0` binds to conflicting values, so there is no viable join.
        let a = subst_of(vec![(0, val(0))]);
        let b = subst_of(vec![(0, val(1))]);
        assert!(a.join_with(&b).is_none());
    }

    #[test]
    fn match_substitution_mult_with() {
        let a = subst_of(vec![(0, val(0))]);
        let others = vec![
            subst_of(vec![(1, val(1))]), // compatible -> kept
            subst_of(vec![(0, val(9))]), // conflicts on key 0 -> filtered out
        ];
        let result = a.mult_with(&others);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].inner.len(), 2);
    }
}
