use std::{collections::BTreeMap, fmt::Display, iter::zip};

use crate::{
    ast::{Clause, Cons, Index, Literal, Tuple, TypedCore, ValueAddressOrValue, Var},
    state_space::{Closure, Value, ValueAddress},
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
            // TODO value lists and patterns for them are not supported yet
            if clauses[i].pats.inner.len() != 1 {
                panic!("cs_match_vaddr: Expected clauses[i].pats to be of length 1 (Everything is a singleton ASTLIST). Counter-example found: {}", clauses[i].pats)
            }

            let substs = Self::p_match_vaddr(
                (*clauses[i].pats.inner)[0].as_pattern(),
                v_addr,
                value_store,
                ast_helper,
            );

            // TODO REVISIT
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
            // NOTE it seems like everything is just a singleton ASTList
            // TODO [AP] Correction: pats is probably an ast_list for several patterns that, if any
            // of them matches, lead to the same body. I will keep the panic for now, but will
            // revisit the issue
            if clauses[i].pats.inner.len() != 1 {
                panic!("cs_match_value: Expected clauses[i].pats to be of length 1 (Everything is a singleton ASTLIST). Counter-example found: {}", clauses[i].pats)
            }

            let substs = Self::p_match_value(
                &(*clauses[i].pats.inner)[0].as_pattern(),
                value,
                value_store,
                ast_helper,
            );

            // TODO REVISIT
            if substs.len() > 0 {
                if Self::match_guard(&*clauses[i].guard, value_store, ast_helper) {
                    clause_matches.insert(i, substs);
                }
            }
        }

        clause_matches
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
                        let pairs = zip(pc.iter().map(|tc| tc.as_pattern()), vc);

                        // substs is a
                        let mut substs: Vec<MatchSubstitution<V>> = Vec::new();

                        for (p, v) in pairs {
                            let value = Value::Closure(Closure {
                                prog_loc: (*v).get_index().unwrap(),
                                env: v_clo.env.clone(),
                            });
                            let res = Self::p_match_value(&p, &value, value_store, ast_helper);

                            if res.is_empty() {
                                substs = Vec::new();
                                break;
                            }

                            // product with all previous substs
                            if substs.is_empty() {
                                // for first iteration
                                substs = res;
                            } else {
                                // for any other iteration
                                substs = substs
                                    .iter()
                                    .flat_map(|subst| subst.mult_with(&res))
                                    .collect()
                            }

                            // current element
                            // TODO continue here
                            // match curr_p {
                            //     PatternKind::Var(v) => {
                            //         // make v_addr for curr_v_tc: new_vaddr(curr_v_tc)
                            //         // insert into value_store: value_store[v_addr |-> { curr_v_tc }]
                            //         // add substitution: v |-> v_addr
                            //         let mut subst = MatchSubstitution::new();
                            //     },
                            //     PatternKind::Literal(pl) => match curr_v_tc {
                            //         TypedCore::Literal(vl) => if Self::literal_cmp(vl, pl) { Vec::from([MatchSubstitution::new()]) } else { Vec::new() },
                            //         _ => Vec::new(), // can't match literal to anything other than a
                            //                         // literal
                            //     },
                            //     PatternKind::Cons(c) => todo!(),
                            //     PatternKind::Tuple(t) => todo!(),
                            // };

                            // match next_p {
                            //     // Either literal [] or cons
                            //     // TODO should I enforce that or just blindly match whatever I
                            //     // see?
                            //     PatternKind::Literal(p_l) => match &(*p_l.val) {
                            //         // TODO missing match to value !!!!!
                            //         TypedCore::AstList(p_al) => {
                            //             if p_al.inner.len() != 0 {
                            //                 panic!("p_match_value: Unexpected length of literal list in cons: {}", p_al)
                            //             }
                            //
                            //             break;
                            //         },
                            //         _ => panic!("p_match_value: Unexpected literal in cons: {}", p_l)
                            //     },
                            //     PatternKind::Cons(p_cons) => match next_v_tc {
                            //         TypedCore::Cons(v_cons) => {
                            //             curr_p = &(*p_cons.hd).as_pattern();
                            //             next_p = &(*p_cons.tl).as_pattern();
                            //
                            //             curr_v_tc = &(*v_cons.hd);
                            //             next_v_tc = &(*v_cons.tl);
                            //         },
                            //         _ => {
                            //             // pattern and value do not agree in length -- fail entire
                            //             // pattern match
                            //             Vec::new();
                            //             break;
                            //         }
                            //     },
                            //     _ => panic!("p_match_value: Unexpected pattern kind in cons: {}", next_p)
                            // };
                        }

                        substs
                    }
                    _ => Vec::new(), // can't match cons to anything other than cons
                },
                Value::Pid(_) => Vec::new(), // can't match cons to pid
            },
            PatternKind::Tuple(pt) => match value {
                Value::Closure(v_clo) => match ast_helper.get(v_clo.prog_loc) {
                    TypedCore::Tuple(v_tup) => {
                        todo!("p_match_value: Tuple pattern not supported yet")
                    }
                    _ => Vec::new(), // can't match tuple to anything other than tuple
                },
                Value::Pid(_) => Vec::new(), // can"t match tuple to pid
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
    // pub fn cmatch_value<V: ValueAddress>(
    //     clause: &Clause,
    //     value: &Value<V>,
    //     value_store: &SetMap<V, Value<V>>,
    //     ast_helper: &AstHelper,
    // ) -> Vec<MatchSubstitution<V>> {
    //     // TODO why a vector of substitutions?
    //     match value {
    //         Value::Closure(clo) => {
    //             match ast_helper.get(clo.prog_loc) {
    //                 TypedCore::Var(_) | TypedCore::Literal(_) => {
    //                     if clause.pats.inner.len() != 1 {
    //                         // there should only be one pattern
    //                         return Vec::new();
    //                     }
    //
    //                     Self::pmatch_value(&clause.pats.inner[0], value, value_store, ast_helper)
    //                 }
    //                 TypedCore::AstList(al) => {
    //                     Self::cmatch_list(clause, al, &clo.env, value_store, ast_helper)
    //                 }
    //                 TypedCore::Tuple(tup) => {
    //                     Self::cmatch_list(clause, &tup.es, &clo.env, value_store, ast_helper)
    //                 }
    //                 _ => Vec::new(),
    //             }
    //         }
    //         Value::Pid(_pid) => {
    //             if clause.pats.inner.len() != 1 {
    //                 // there should only be one pattern
    //                 return Vec::new();
    //             }
    //
    //             Self::pmatch_value(&clause.pats.inner[0], value, value_store, ast_helper)
    //         }
    //     }
    // }

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
    // fn cmatch_list<V: ValueAddress>(
    //     clause: &Clause,
    //     value: &AstList<TypedCore>,
    //     env: &Env<V>,
    //     value_store: &SetMap<V, Value<V>>,
    //     ast_helper: &AstHelper,
    // ) -> Vec<MatchSubstitution<V>> {
    //     //TODO dont flatten the structure
    //     let patterns = if clause.pats.inner.len() == 1 {
    //         if let TypedCore::Tuple(tup) = &clause.pats.inner[0] {
    //             &tup.es
    //         } else {
    //             &clause.pats
    //         }
    //     } else {
    //         &clause.pats
    //     };
    //
    //     if value.inner.len() != patterns.inner.len() {
    //         return Vec::new();
    //     }
    //
    //     let mut overall_substs = Vec::new();
    //     for i in 0..value.inner.len() {
    //         let substs_i = &Self::pmatch_value(
    //             &patterns.inner[i],
    //             &Value::Closure(Closure {
    //                 prog_loc: value.inner[i].get_index().unwrap(),
    //                 env: env.clone(),
    //             }),
    //             value_store,
    //             ast_helper,
    //         );
    //
    //         let mut overall_subst_i = MatchSubstitution::new();
    //         for subst in substs_i {
    //             overall_subst_i = overall_subst_i.join_with(subst);
    //         }
    //         if overall_subst_i.inner.len() != 0 {
    //             overall_substs.push(overall_subst_i);
    //         }
    //     }
    //     overall_substs
    // }

    /// Matches a value against a pattern
    ///
    /// ## Arguments
    /// * `pattern` - a singular pattern (from a clause of `case` or `receive`)
    /// * `value` - a singular value
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
    // fn p_match_value<V: ValueAddress>(
    //     pattern: &TypedCore,
    //     value: &Value<V>,
    //     value_store: &SetMap<V, Value<V>>,
    //     ast_helper: &AstHelper,
    // ) -> Vec<MatchSubstitution<V>> {
    // match value {
    //     Value::Closure(clo) => match &ast_helper.get(clo.prog_loc) {
    //         TypedCore::Var(v) => {
    //             let values = value_store
    //                 .get(clo.env.inner.get(v.var_id.unwrap()).unwrap())
    //                 .unwrap();
    //
    //             let mut new_substs = Vec::new();
    //             for value in values {
    //                 new_substs.append(&mut Self::pmatch_value(
    //                     &pattern,
    //                     value,
    //                     value_store,
    //                     ast_helper,
    //                 ));
    //             }
    //             new_substs
    //         }
    //         TypedCore::Literal(val_l) => match &pattern {
    //             TypedCore::Literal(pattern_l) => {
    //                 if let TypedCore::String(val_s) = &*val_l.val {
    //                     if let TypedCore::String(pattern_s) = &*pattern_l.val {
    //                         if val_s.inner == pattern_s.inner {
    //                             Vec::from([MatchSubstitution::new()])
    //                         } else {
    //                             Vec::new()
    //                         }
    //                     } else {
    //                         return Vec::new();
    //                     }
    //                 } else {
    //                     return Vec::new();
    //                 }
    //             }
    //             _ => Vec::new(),
    //         },
    //         // NOTE should be TypedCore::Cons in a future version
    //         TypedCore::AstList(al_value) => match pattern {
    //             TypedCore::AstList(al_pattern) => {
    //                 if al_value.inner.len() != al_pattern.inner.len() {
    //                     return Vec::new();
    //                 }
    //
    //                 // pattern match elements
    //                 let new_substs;
    //                 for i in 0..al_value.inner.len() {
    //                     // TODO CURRENT FRONTIER
    //                     // Should think about rewriting pmatch_value to work with value:
    //                     // TypedCore instead of value: Value
    //                     new_substs.join_with(Self::pmatch_value(
    //                         &al_pattern.inner[i],
    //                         &al_value.inner[i],
    //                         value_store,
    //                         ast_helper,
    //                     ));
    //                 }
    //
    //                 new_substs
    //             }
    //             _ => Vec::new(),
    //         },
    //         TypedCore::Tuple(al) => Vec::new(),
    //     },
    //     Value::Pid(_) => Vec::new(), // unmatchable if the pattern is not a var
    // }

    // OLD
    // ================================
    // NEW

    //     match pattern {
    //         TypedCore::Var(v) => {
    //             // wildcard pattern -> create binding
    //             let mut new_subst = MatchSubstitution::new();
    //             new_subst.inner.insert(v.var_id.clone(), value.clone());
    //             Vec::from([new_subst])
    //         }
    //         TypedCore::Literal(pattern_l) => match value {
    //             // literal pattern -> match literals
    //             Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
    //                 TypedCore::Literal(msg_l) => {
    //                     if Self::literal_cmp(msg_l, pattern_l) {
    //                         Vec::from([MatchSubstitution::new()])
    //                     } else {
    //                         Vec::new()
    //                     }
    //                 }
    //                 _ => Vec::new(), // must be literal closure to match
    //             },
    //             _ => Vec::new(), // must be literal closure to match
    //         },
    //         TypedCore::Cons(c_pat) => match value {
    //             // construct pattern -> descend per sub-pattern
    //             Value::Closure(clo) => match &ast_helper.get(clo.prog_loc) {
    //                 TypedCore::Cons(c_val) => match &*c_pat.hd {
    //                     // NOTE Probably should be something other than null
    //                     TypedCore::Null(_) => match &*c_val.hd {
    //                         TypedCore::Null(_) => Vec::from([MatchSubstitution::new()]), // matches
    //                         _ => Vec::new(), // length mismatch
    //                     },
    //                     TypedCore::Var(_) => {
    //                         // match head
    //                         pmatch_value(
    //
    //                         // then match tail
    //
    //                     }
    //                 },
    //                 // match &*c_pat.tl {
    //                 // // tail is either cons or nil
    //                 // TypedCore::Cons(c_pat_tl) => {
    //                 //
    //                 // }
    //                 // TypedCore::Null(_) => Vec::new(),
    //                 // _ => panic!(),
    //                 // },
    //                 _ => Vec::new(), // must be cons closure to match
    //             },
    //             _ => Vec::new(), // must be cons closure to match
    //         },
    //         TypedCore::Tuple(tup) => match value {
    //             Value::Closure(clo) => match ast_helper.get(clo.prog_loc) {
    //                 TypedCore::Tuple(tup) => {}
    //                 _ => Vec::new(), // must be tuple closure to match
    //             },
    //             _ => Vec::new(), // must be tuple closure to match
    //         },
    //     }
    // }

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
}
impl Display for PatternKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            PatternKind::Var(v) => v.fmt(f),
            PatternKind::Cons(c) => c.fmt(f),
            PatternKind::Literal(l) => l.fmt(f),
            PatternKind::Tuple(t) => t.fmt(f),
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
            _ => panic!("Invalid typed core for pattern. Should already have been handled by the erlang compiler.")
        }
    }
}
