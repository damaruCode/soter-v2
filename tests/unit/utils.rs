// ---------------
// Small AST constructors to keep the tests below readable
// ---------------

use soter_v2::ast::*;

pub fn str_lit(s: &str) -> TypedCore {
    let mut lit = Literal::new();
    lit.val = Box::new(TypedCore::String(ErlString::new(s.to_string())));
    TypedCore::Literal(lit)
}

pub fn bool_lit(b: bool) -> TypedCore {
    let mut lit = Literal::new();
    lit.val = Box::new(TypedCore::Bool(ErlBool::new(b)));
    TypedCore::Literal(lit)
}

pub fn list_lit(elems: Vec<TypedCore>) -> TypedCore {
    let mut lit = Literal::new();
    lit.val = Box::new(TypedCore::AstList(AstList::from(elems)));
    TypedCore::Literal(lit)
}

pub fn empty_list_lit() -> TypedCore {
    let mut lit = Literal::new();
    lit.val = Box::new(TypedCore::AstList(AstList::new()));
    TypedCore::Literal(lit)
}

pub fn var(name: &str) -> TypedCore {
    let mut v = Var::new();
    v.name = Box::new(TypedCore::String(ErlString::new(name.to_string())));
    TypedCore::Var(v)
}

/// A proper list `[e_1, ..., e_n]` built from nested `Cons` cells ending in the literal empty list `[]`.
pub fn list_of(elems: Vec<TypedCore>) -> TypedCore {
    let mut acc = empty_list_lit();
    for elem in elems.into_iter().rev() {
        let mut cons = Cons::new();
        cons.hd = Box::new(elem);
        cons.tl = Box::new(acc);
        acc = TypedCore::Cons(cons);
    }
    acc
}

pub fn tuple_of(elems: Vec<TypedCore>) -> TypedCore {
    let mut tuple = Tuple::new();
    tuple.es = AstList::new();
    for elem in elems {
        tuple.es.inner.push(elem);
    }
    TypedCore::Tuple(tuple)
}

pub fn clause(pat: TypedCore, guard: TypedCore) -> Clause {
    let mut clause = Clause::new();
    clause.pats = AstList::new();
    clause.pats.inner.push(pat);
    clause.guard = Box::new(guard);
    clause
}

pub fn true_clause(pat: TypedCore) -> Clause {
    clause(pat, bool_lit(true))
}
