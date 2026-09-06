use std::collections::{BTreeMap, VecDeque};

use soter_v2::abstraction::standard::{StandardAbstraction, VAddr};
use soter_v2::abstraction::Abstraction;
use soter_v2::analyzer::{MatchHelper, MatchSubstitution};
use soter_v2::ast::*;
use soter_v2::state_space::{Closure, Env, Pid, ProcState, ProgLocOrPid, Store, Time, Value};
use soter_v2::util::AstHelper;

// ---------------
// Small AST constructors to keep the tests below readable
// ---------------

fn str_lit(s: &str) -> TypedCore {
    let mut lit = Literal::new();
    lit.val = Box::new(TypedCore::String(ErlString::new(s.to_string())));
    TypedCore::Literal(lit)
}

fn bool_lit(b: bool) -> TypedCore {
    let mut lit = Literal::new();
    lit.val = Box::new(TypedCore::Bool(ErlBool::new(b)));
    TypedCore::Literal(lit)
}

fn empty_list_lit() -> TypedCore {
    let mut lit = Literal::new();
    lit.val = Box::new(TypedCore::AstList(AstList::new()));
    TypedCore::Literal(lit)
}

fn var(name: &str) -> TypedCore {
    let mut v = Var::new();
    v.name = Box::new(TypedCore::String(ErlString::new(name.to_string())));
    TypedCore::Var(v)
}

/// A proper list `[e_1, ..., e_n]` built from nested `Cons` cells ending in the literal empty list `[]`.
fn list_of(elems: Vec<TypedCore>) -> TypedCore {
    let mut acc = empty_list_lit();
    for elem in elems.into_iter().rev() {
        let mut cons = Cons::new();
        cons.hd = Box::new(elem);
        cons.tl = Box::new(acc);
        acc = TypedCore::Cons(cons);
    }
    acc
}

fn tuple_of(elems: Vec<TypedCore>) -> TypedCore {
    let mut tuple = Tuple::new();
    tuple.es = AstList::new();
    for elem in elems {
        tuple.es.inner.push(elem);
    }
    TypedCore::Tuple(tuple)
}

fn clause(pat: TypedCore, guard: TypedCore) -> Clause {
    let mut clause = Clause::new();
    clause.pats = AstList::new();
    clause.pats.inner.push(pat);
    clause.guard = Box::new(guard);
    clause
}

fn true_clause(pat: TypedCore) -> Clause {
    clause(pat, bool_lit(true))
}

// ---------------
// cs_match_vaddr
// ---------------

/// Matches `source_val` against a single clause whose pattern is `source_val`
/// itself (an identity match). Clause 0 must match.
fn test_cs_match_vaddr_id(source_val: TypedCore) {
    let clause_substs = run_cs_match_vaddr(
        source_val.clone(),
        vec![true_clause(source_val)],
        simple_closure,
    );

    assert!(
        clause_substs.contains_key(&0),
        "Expected clause 0 to match, but substitutions do not include it: {:?}",
        clause_substs
    );
}

#[test]
pub fn test_cs_match_vaddr_var_to_var() {
    // `case S of <D> -> ...`: the variable pattern `D` must capture whatever `S`
    // holds, so `source_val` has to flow into `D`.
    let source_val = Value::Closure(Closure {
        prog_loc: 0,
        env: Env::init(),
    });

    with_indexed_case(
        // The `let` argument is irrelevant here; the source address is seeded directly.
        str_lit("a"),
        vec![true_clause(var("D"))],
        |ast_helper, clauses, _arg_index, source_var_id| {
            let abstraction = StandardAbstraction::new(10);
            let mut store = Store::init(abstraction.stop_kaddr());
            let proc_state = ProcState::init(abstraction.stop_kaddr());

            let source_vaddr = abstraction.new_vaddr(
                &proc_state,
                source_var_id,
                &ProgLocOrPid::ProgLoc(0),
                &Env::init(),
                &Time {
                    inner: VecDeque::from([0]),
                },
            );
            store.value.push(source_vaddr.clone(), source_val.clone());

            let clause_substs =
                MatchHelper::cs_match_vaddr(clauses, &source_vaddr, &store.value, ast_helper);

            let TypedCore::Var(indexed_dest_var) = &clauses[0].pats.inner[0] else {
                panic!("Expected var pattern");
            };
            let dest_id = *indexed_dest_var.var_id.unwrap();

            // check that SOURCE flows into DESTINATION
            for substs in clause_substs.values() {
                for subst in substs {
                    match subst.inner.get(&dest_id).unwrap() {
                        ValueAddressOrValue::ValueAddress(dest_vaddr) => {
                            if store
                                .value
                                .get(dest_vaddr)
                                .unwrap()
                                .iter()
                                .any(|dest_val| *dest_val == source_val)
                            {
                                return;
                            }
                        }
                        ValueAddressOrValue::Value(dest_val) => {
                            assert_eq!(*dest_val, source_val);
                            return;
                        }
                    }
                }
            }
            panic!("{:?}", clause_substs);
        },
    );
}

#[test]
pub fn test_cs_match_vaddr_lit() {
    test_cs_match_vaddr_id(str_lit("a"));
}

#[test]
pub fn test_cs_match_vaddr_cons() {
    test_cs_match_vaddr_id(list_of(vec![str_lit("a"), str_lit("b"), str_lit("c")]));
}

#[test]
pub fn test_cs_match_vaddr_tuple() {
    test_cs_match_vaddr_id(tuple_of(vec![str_lit("a"), str_lit("b"), str_lit("c")]));
}

#[test]
fn test_cs_match_vaddr_multiple_values_partial() {
    // The source address abstracts *two* distinct values, `'a'` and `'b'`. Each of
    // the two literal clauses matches exactly one of them, so both must be selected.
    // The tuple argument is only a convenient holder of two indexed literals to point
    // the seeded closures at.
    with_indexed_case(
        tuple_of(vec![str_lit("a"), str_lit("b")]),
        vec![true_clause(str_lit("a")), true_clause(str_lit("b"))],
        |ast_helper, clauses, arg_index, source_var_id| {
            let TypedCore::Tuple(indexed_tuple) = ast_helper.get(arg_index) else {
                panic!("Expected the argument to be a tuple");
            };
            let a_index = indexed_tuple.es.inner[0].get_index().expect("indexed 'a'");
            let b_index = indexed_tuple.es.inner[1].get_index().expect("indexed 'b'");

            let abstraction = StandardAbstraction::new(10);
            let mut store = Store::init(abstraction.stop_kaddr());
            let proc_state = ProcState::init(abstraction.stop_kaddr());

            let source_vaddr = abstraction.new_vaddr(
                &proc_state,
                source_var_id,
                &ProgLocOrPid::ProgLoc(0),
                &Env::init(),
                &Time {
                    inner: VecDeque::from([0]),
                },
            );

            store.value.push(
                source_vaddr.clone(),
                Value::Closure(Closure {
                    prog_loc: a_index,
                    env: Env::init(),
                }),
            );
            store.value.push(
                source_vaddr.clone(),
                Value::Closure(Closure {
                    prog_loc: b_index,
                    env: Env::init(),
                }),
            );

            let clause_substs =
                MatchHelper::cs_match_vaddr(clauses, &source_vaddr, &store.value, ast_helper);

            assert_eq!(matched(&clause_substs), vec![0, 1]);
        },
    );
}

// ---------------
// Shared driver: builds `let S = <source_arg> in case S of <clauses> end`, indexes
// it, and hands the indexed pieces to a callback that performs the actual match.
// ---------------
fn with_indexed_case<R>(
    source_arg: TypedCore,
    clauses: Vec<Clause>,
    f: impl FnOnce(&AstHelper, &Vec<Clause>, usize, usize) -> R,
) -> R {
    let mut source_var = Var::new();
    source_var.name = Box::new(TypedCore::String(ErlString::new(String::from("S"))));

    let mut let_var = Let::new();
    let_var.vars = AstList::new();
    let_var.vars.inner.push(TypedCore::Var(source_var.clone()));
    let_var.arg = Box::new(source_arg);

    let mut case = Case::new();
    case.arg = Box::new(TypedCore::Var(source_var.clone()));
    case.clauses = AstList::new();
    for clause in clauses {
        case.clauses.inner.push(TypedCore::Clause(clause));
    }
    let_var.body = Box::new(TypedCore::Case(case));

    let mut ast_helper = AstHelper::new();
    let indexed_ast_tc = ast_helper.build_indecies(TypedCore::Let(let_var));
    ast_helper.build_lookup(&indexed_ast_tc);

    let TypedCore::Let(indexed_let_var) = &indexed_ast_tc else {
        panic!("expected a let expression");
    };
    let TypedCore::Var(indexed_source_var) = &indexed_let_var.vars.inner[0] else {
        panic!("expected a source variable");
    };
    let TypedCore::Case(indexed_case) = &*indexed_let_var.body else {
        panic!("expected a case expression");
    };

    let arg_index = indexed_let_var
        .arg
        .get_index()
        .expect("let argument should have been indexed");
    let source_var_id = *indexed_source_var.var_id.unwrap();
    let indexed_clauses = Vec::from(&indexed_case.clauses);

    f(&ast_helper, &indexed_clauses, arg_index, source_var_id)
}

/// A single closure value pointing at the (indexed) `let` argument, with an empty env.
fn simple_closure(arg_index: usize) -> Vec<Value<VAddr>> {
    vec![Value::Closure(Closure {
        prog_loc: arg_index,
        env: Env::init(),
    })]
}

fn run_cs_match_vaddr(
    source_arg: TypedCore,
    clauses: Vec<Clause>,
    make_values: impl Fn(usize) -> Vec<Value<VAddr>>,
) -> BTreeMap<usize, Vec<MatchSubstitution<VAddr>>> {
    with_indexed_case(
        source_arg,
        clauses,
        |ast_helper, clauses, arg_index, source_var_id| {
            let abstraction = StandardAbstraction::new(10);
            let mut store = Store::init(abstraction.stop_kaddr());
            let proc_state = ProcState::init(abstraction.stop_kaddr());

            let source_vaddr = abstraction.new_vaddr(
                &proc_state,
                source_var_id,
                &ProgLocOrPid::ProgLoc(0),
                &Env::init(),
                &Time {
                    inner: VecDeque::from([0]),
                },
            );

            for value in make_values(arg_index) {
                store.value.push(source_vaddr.clone(), value);
            }

            MatchHelper::cs_match_vaddr(clauses, &source_vaddr, &store.value, ast_helper)
        },
    )
}

fn run_cs_match_value(
    source_arg: TypedCore,
    clauses: Vec<Clause>,
) -> BTreeMap<usize, Vec<MatchSubstitution<VAddr>>> {
    with_indexed_case(
        source_arg,
        clauses,
        |ast_helper, clauses, arg_index, _source_var_id| {
            let abstraction = StandardAbstraction::new(10);
            let store = Store::init(abstraction.stop_kaddr());

            let value = Value::Closure(Closure {
                prog_loc: arg_index,
                env: Env::init(),
            });

            MatchHelper::cs_match_value(clauses, &value, &store.value, ast_helper)
        },
    )
}

/// The clause indices that matched, in ascending order.
fn matched(substs: &BTreeMap<usize, Vec<MatchSubstitution<VAddr>>>) -> Vec<usize> {
    substs.keys().copied().collect()
}

// ---------------
// cs_match_vaddr: negative / mismatch cases
// ---------------
#[test]
fn test_cs_match_vaddr_lit_mismatch() {
    let substs = run_cs_match_vaddr(
        str_lit("a"),
        vec![true_clause(str_lit("b"))],
        simple_closure,
    );
    assert!(substs.is_empty());
}

#[test]
fn test_cs_match_vaddr_cons_arity_mismatch() {
    let substs = run_cs_match_vaddr(
        list_of(vec![str_lit("a"), str_lit("b"), str_lit("c")]),
        vec![true_clause(list_of(vec![str_lit("a"), str_lit("b")]))],
        simple_closure,
    );
    assert!(substs.is_empty());
}

#[test]
fn test_cs_match_vaddr_tuple_arity_mismatch() {
    let substs = run_cs_match_vaddr(
        tuple_of(vec![str_lit("a"), str_lit("b")]),
        vec![true_clause(tuple_of(vec![
            str_lit("a"),
            str_lit("b"),
            str_lit("c"),
        ]))],
        simple_closure,
    );
    assert!(substs.is_empty());
}

#[test]
fn test_cs_match_vaddr_type_mismatch() {
    // A tuple value can never match a cons pattern.
    let substs = run_cs_match_vaddr(
        tuple_of(vec![str_lit("a")]),
        vec![true_clause(list_of(vec![str_lit("a")]))],
        simple_closure,
    );
    assert!(substs.is_empty());
}

#[test]
fn test_cs_match_vaddr_pid() {
    // A pid never matches a literal, but does match a variable pattern.
    let substs = run_cs_match_vaddr(
        str_lit("a"), // unused: the value is overridden to a pid below
        vec![true_clause(str_lit("a")), true_clause(var("X"))],
        |_arg_index| vec![Value::Pid(Pid::init())],
    );
    assert_eq!(matched(&substs), vec![1]);
}

// ---------------
// cs_match_vaddr: clause selection across multiple clauses
// ---------------

#[test]
fn test_cs_match_vaddr_second_clause() {
    // The first clause mismatches; only the second is selected.
    let substs = run_cs_match_vaddr(
        str_lit("a"),
        vec![true_clause(str_lit("b")), true_clause(str_lit("a"))],
        simple_closure,
    );
    assert_eq!(matched(&substs), vec![1]);
}

#[test]
fn test_cs_match_vaddr_multiple_matching() {
    // A literal clause and a catch-all variable clause both match.
    let substs = run_cs_match_vaddr(
        str_lit("a"),
        vec![true_clause(str_lit("a")), true_clause(var("X"))],
        simple_closure,
    );
    assert_eq!(matched(&substs), vec![0, 1]);
}

// ---------------
// cs_match_vaddr: guards
// ---------------

#[test]
fn test_cs_match_vaddr_guard_false() {
    // The pattern matches but the `false` guard excludes the clause.
    let substs = run_cs_match_vaddr(
        str_lit("a"),
        vec![clause(str_lit("a"), bool_lit(false))],
        simple_closure,
    );
    assert!(substs.is_empty());
}

// ---------------
// cs_match_vaddr: nested and empty-list patterns
// ---------------

#[test]
fn test_cs_match_vaddr_nested() {
    // `{a, [b, c]}` matched against an identical pattern recurses through compounds.
    let nested = tuple_of(vec![
        str_lit("a"),
        list_of(vec![str_lit("b"), str_lit("c")]),
    ]);
    let substs = run_cs_match_vaddr(nested.clone(), vec![true_clause(nested)], simple_closure);
    assert_eq!(matched(&substs), vec![0]);
}

#[test]
fn test_cs_match_vaddr_empty_list_match() {
    let substs = run_cs_match_vaddr(
        empty_list_lit(),
        vec![true_clause(empty_list_lit())],
        simple_closure,
    );
    assert_eq!(matched(&substs), vec![0]);
}

#[test]
fn test_cs_match_vaddr_empty_list_mismatch() {
    // A non-empty list is a `Cons`, which cannot match the `[]` literal pattern.
    let substs = run_cs_match_vaddr(
        list_of(vec![str_lit("a")]),
        vec![true_clause(empty_list_lit())],
        simple_closure,
    );
    assert!(substs.is_empty());
}

#[test]
fn test_cs_match_vaddr_cons_with_var_head() {
    let mut source_cons = Cons::new();
    source_cons.hd = Box::new(var("X"));
    source_cons.tl = Box::new(empty_list_lit());

    let mut pat_cons = Cons::new();
    pat_cons.hd = Box::new(var("P"));
    pat_cons.tl = Box::new(empty_list_lit());

    let mut source_var = Var::new();
    source_var.name = Box::new(TypedCore::String(ErlString::new(String::from("S"))));

    let mut case = Case::new();
    case.arg = Box::new(TypedCore::Var(source_var.clone()));
    case.clauses = AstList::new();
    case.clauses
        .inner
        .push(TypedCore::Clause(true_clause(TypedCore::Cons(pat_cons))));

    let mut let_s = Let::new();
    let_s.vars = AstList::new();
    let_s.vars.inner.push(TypedCore::Var(source_var.clone()));
    let_s.arg = Box::new(TypedCore::Cons(source_cons));
    let_s.body = Box::new(TypedCore::Case(case));

    let mut let_x = Let::new();
    let_x.vars = AstList::new();
    let_x.vars.inner.push(var("X"));
    let_x.arg = Box::new(str_lit("a"));
    let_x.body = Box::new(TypedCore::Let(let_s));

    let mut ast_helper = AstHelper::new();
    let indexed_ast_tc = ast_helper.build_indecies(TypedCore::Let(let_x));
    ast_helper.build_lookup(&indexed_ast_tc);

    let TypedCore::Let(indexed_let_x) = &indexed_ast_tc else {
        panic!("expected let X = ...");
    };
    let TypedCore::Var(indexed_x) = &indexed_let_x.vars.inner[0] else {
        panic!("expected the `X` binder");
    };
    let x_id = *indexed_x.var_id.unwrap();
    let a_index = indexed_let_x
        .arg
        .get_index()
        .expect("let argument should have been indexed");

    let TypedCore::Let(indexed_let_s) = &*indexed_let_x.body else {
        panic!("expected the let S =... let");
    };
    let TypedCore::Var(indexed_source_var) = &indexed_let_s.vars.inner[0] else {
        panic!("expected the `S` binder");
    };
    let source_var_id = *indexed_source_var.var_id.unwrap();
    let cons_index = indexed_let_s
        .arg
        .get_index()
        .expect("inner let argument should have been indexed");

    let TypedCore::Case(indexed_case) = &*indexed_let_s.body else {
        panic!("expected the case");
    };
    let TypedCore::Clause(indexed_clause) = &indexed_case.clauses.inner[0] else {
        panic!("expected a clause");
    };
    let TypedCore::Cons(indexed_pat_cons) = &indexed_clause.pats.inner[0] else {
        panic!("expected the pattern to be a cons");
    };
    let TypedCore::Var(indexed_p) = &*indexed_pat_cons.hd else {
        panic!("expected a variable pattern head");
    };
    let p_id = *indexed_p.var_id.unwrap();

    let abstraction = StandardAbstraction::new(10);
    let mut store = Store::init(abstraction.stop_kaddr());
    let proc_state = ProcState::init(abstraction.stop_kaddr());

    let source_vaddr = abstraction.new_vaddr(
        &proc_state,
        source_var_id,
        &ProgLocOrPid::ProgLoc(0),
        &Env::init(),
        &Time {
            inner: VecDeque::from([0]),
        },
    );
    let x_vaddr = abstraction.new_vaddr(
        &proc_state,
        x_id,
        &ProgLocOrPid::ProgLoc(0),
        &Env::init(),
        &Time {
            inner: VecDeque::from([0]),
        },
    );
    // `X` holds the literal `'a'`.
    store.value.push(
        x_vaddr.clone(),
        Value::Closure(Closure {
            prog_loc: a_index,
            env: Env::init(),
        }),
    );

    // The source cons closure captures `X` in its environment.
    let mut env = Env::init();
    env.inner.insert(x_id, x_vaddr.clone());
    store.value.push(
        source_vaddr.clone(),
        Value::Closure(Closure {
            prog_loc: cons_index,
            env,
        }),
    );

    let indexed_clauses = Vec::from(&indexed_case.clauses);
    let substs =
        MatchHelper::cs_match_vaddr(&indexed_clauses, &source_vaddr, &store.value, &ast_helper);

    let clause_substs = substs.get(&0).expect("clause 0 should match");
    assert!(clause_substs.iter().any(|subst| {
        matches!(
            subst.inner.get(&p_id),
            Some(ValueAddressOrValue::ValueAddress(bound)) if *bound == x_vaddr
        )
    }));
}

// ---------------
// cs_match_value
// ---------------

#[test]
fn test_cs_match_value_lit() {
    let substs = run_cs_match_value(str_lit("a"), vec![true_clause(str_lit("a"))]);
    assert_eq!(matched(&substs), vec![0]);
}

#[test]
fn test_cs_match_value_mismatch() {
    let substs = run_cs_match_value(str_lit("a"), vec![true_clause(str_lit("b"))]);
    assert!(substs.is_empty());
}
