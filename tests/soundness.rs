use serde_json::Number;
use soter_v2::abstraction::standard::StandardAbstraction;
use soter_v2::abstraction::standard::VAddr;
use soter_v2::analyzer::Analyzer;
use soter_v2::analyzer::MatchHelper;
use soter_v2::ast;
use soter_v2::ast::AstList;
use soter_v2::ast::Clause;
use soter_v2::ast::Cons;
use soter_v2::ast::ErlNumber;
use soter_v2::ast::ErlString;
use soter_v2::ast::Literal;
use soter_v2::ast::MaybeIndex;
use soter_v2::ast::Tuple;
use soter_v2::ast::TypedCore;
use soter_v2::ast::Var;
use soter_v2::erlang;
use soter_v2::state_space::Value;
use soter_v2::state_space::VarName;
use soter_v2::util::AstHelper;
use soter_v2::util::SetMap;

#[derive(Debug, Clone)]
enum P<'p> {
    Var,               // TypedCore::Var
    Literal(&'p str),  // TypedCore::Literal
    List(Vec<P<'p>>),  // TypedCore::Cons
    Tuple(Vec<P<'p>>), // TypedCore::Tuple
}

impl From<P<'_>> for AstList<TypedCore> {
    fn from(pattern: P) -> Self {
        fn resolve(pattern: P) -> TypedCore {
            match pattern {
                P::Var => TypedCore::Var(Var {
                    anno: AstList::new(),
                    name: Box::new(TypedCore::Number(ErlNumber {
                        inner: Number::from(0),
                        index: MaybeIndex::None,
                    })),
                    var_id: MaybeIndex::None,
                    index: MaybeIndex::None,
                }),
                P::Literal(s) => TypedCore::Literal(Literal {
                    anno: AstList::new(),
                    val: Box::new(TypedCore::String(ErlString {
                        inner: String::from(s),
                        index: MaybeIndex::None,
                    })),
                    index: MaybeIndex::None,
                }),
                P::List(v) => {
                    // [p1, p2, ..., pn] desugars to cons(p1, cons(p2, ... cons(pn, nil) ...)),
                    // matching how `TypedCore::Cons` actually represents Erlang lists.
                    let mut tail = TypedCore::Literal(Literal {
                        anno: AstList::new(),
                        val: Box::new(TypedCore::AstList(AstList {
                            inner: Vec::new(),
                            index: MaybeIndex::None,
                        })),
                        index: MaybeIndex::None,
                    });

                    for pattern in v.into_iter().rev() {
                        tail = TypedCore::Cons(Cons {
                            anno: AstList::new(),
                            hd: Box::new(resolve(pattern)),
                            tl: Box::new(tail),
                            index: MaybeIndex::None,
                        });
                    }

                    tail
                }
                P::Tuple(v) => {
                    let mut al = AstList::new();
                    for pattern in v {
                        al.inner.push(resolve(pattern));
                    }
                    TypedCore::Tuple(Tuple {
                        anno: AstList::new(),
                        es: al,
                        index: MaybeIndex::None,
                    })
                }
            }
        }

        let tc = resolve(pattern);
        let mut al = AstList::new();
        al.inner.push(tc);
        al
    }
}

impl From<P<'_>> for Clause {
    fn from(pattern: P) -> Self {
        let clause = Clause {
            anno: AstList::new(),
            pats: AstList::from(pattern),
            guard: Box::new(TypedCore::Literal(Literal {
                anno: AstList::new(),
                val: Box::new(TypedCore::String(ErlString {
                    inner: "true".to_string(),
                    index: MaybeIndex::None,
                })),
                index: MaybeIndex::None,
            })),
            body: Box::new(TypedCore::Dummy),
            index: MaybeIndex::None,
        };
        clause
    }
}

fn contains(
    pattern: P,
    var_name: &str,
    val_store: &SetMap<VAddr, Value<VAddr>>,
    ast_helper: &AstHelper,
) {
    let cvec = vec![Clause::from(pattern.clone())];

    for (vaddr, _val) in &val_store.inner {
        match ast_helper.get(vaddr.var_name) {
            TypedCore::Var(v) => {
                if VarName::from(v) == VarName::Atom(var_name.to_string()) {
                    println!("vaddr {} matches varname {}", vaddr, var_name);
                    let sub = MatchHelper::cs_match_vaddr(&cvec, vaddr, val_store, ast_helper);

                    for (_val, vec) in sub {
                        println!("subst val {} and vec {:#?}", _val, vec);
                        if !vec.is_empty() {
                            return;
                        }
                    }
                }
            }
            tc => panic!("VarId expected to point to AST Var, found {}", tc),
        }
    }
    panic!("No entry {} matching {:#?} found.", var_name, pattern)
}

fn ncontains(
    pattern: P,
    var_name: &str,
    val_store: &SetMap<VAddr, Value<VAddr>>,
    ast_helper: &AstHelper,
) {
    let cvec = vec![Clause::from(pattern)];

    for (vaddr, _val) in &val_store.inner {
        match ast_helper.get(vaddr.var_name) {
            TypedCore::Var(v) => {
                if VarName::from(v) == VarName::Atom(var_name.to_string()) {
                    let sub = MatchHelper::cs_match_vaddr(&cvec, vaddr, val_store, ast_helper);

                    for (_val, vec) in sub {
                        if !vec.is_empty() {
                            panic!();
                        }
                    }
                }
            }
            tc => panic!("VarId expected to point to AST Var, found {}", tc),
        }
    }
}

#[test]
fn test_standard_receive_lit() {
    //erlang::compile();
    erlang::run(&format!("tests/soundness/receive_lit.erl"));
    let core = erlang::get_core(&format!("tests/soundness/receive_lit.erl.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let (_ps, _m, s) = analyzer.run();

    contains(P::Literal("a"), "X", &s.value, &ast_helper);
    ncontains(P::Literal("M"), "X", &s.value, &ast_helper);
}

#[test]
fn test_standard_concurr() {
    //erlang::compile(); //TODO wierd bug when running from uncompiled erlang
    erlang::run(&format!("tests/soundness/concurr.erl"));
    let core = erlang::get_core(&format!("tests/soundness/concurr.erl.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let (_ps, _m, _s) = analyzer.run();
}

#[test]
fn test_standard_id() {
    //erlang::compile();
    erlang::run(&format!("tests/soundness/id.erl"));
    let core = erlang::get_core(&format!("tests/soundness/id.erl.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let (_ps, _m, s) = analyzer.run();

    contains(P::Literal("a"), "X", &s.value, &ast_helper);
    contains(P::Literal("b"), "X", &s.value, &ast_helper);
    ncontains(P::Literal("c"), "X", &s.value, &ast_helper);
}

#[test]
fn test_standard_rec_id() {
    //erlang::compile();
    erlang::run(&format!("tests/soundness/rec_id.erl"));
    let core = erlang::get_core(&format!("tests/soundness/rec_id.erl.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let (_ps, _m, s) = analyzer.run();

    contains(P::Literal("a"), "X", &s.value, &ast_helper);
    contains(P::Literal("b"), "X", &s.value, &ast_helper);
}

#[test]
fn test_pm_var_in_value() {
    erlang::run(&format!("tests/soundness/pm_var_in_value.erl"));
    let core = erlang::get_core(&format!("tests/soundness/pm_var_in_value.erl.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let (_ps, _m, s) = analyzer.run();

    contains(P::Literal("b"), "R", &s.value, &ast_helper);
    ncontains(P::Literal("a"), "R", &s.value, &ast_helper);
}
