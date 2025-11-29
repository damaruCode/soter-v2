use soter_v2::abstraction::standard::StandardAbstraction;
use soter_v2::abstraction::standard::VAddr;
use soter_v2::analyzer::Analyzer;
use soter_v2::analyzer::MatchHelper;
use soter_v2::ast;
use soter_v2::ast::AstList;
use soter_v2::ast::Clause;
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

enum P<'p> {
    Var,               // TypedCore::Var
    Literal(&'p str),  // TypedCore::Literal
    List(Vec<P<'p>>),  // TypedCore::AstList
    Tuple(Vec<P<'p>>), // TypedCore::Tuple
}

impl From<P<'_>> for AstList<TypedCore> {
    fn from(pattern: P) -> Self {
        fn resolve(pattern: P) -> TypedCore {
            match pattern {
                P::Var => TypedCore::Var(Var {
                    anno: AstList::new(),
                    name: Box::new(TypedCore::Dummy),
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
                    let mut al = AstList::new();
                    for pattern in v {
                        al.inner.push(resolve(pattern));
                    }
                    TypedCore::AstList(al)
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

fn equals() -> bool {
    let test = P::List(vec![P::Literal("a"), P::Tuple(vec![P::Var, P::Var])]); // {"a",(X,Y)}
    true
}

fn contains(
    pattern: P,
    var_name: &str,
    val_store: &SetMap<VAddr, Value<VAddr>>,
    ast_helper: &AstHelper,
) {
    let cvec = vec![Clause::from(pattern)];

    for (vaddr, _val) in &val_store.inner {
        if vaddr.var_name == VarName::Atom(var_name.to_string()) {
            let sub = MatchHelper::vmatch(&cvec, vaddr, val_store, ast_helper);

            for (_val, vec) in sub {
                if !vec.is_empty() {
                    return;
                }
            }
        }
    }
    panic!()
}

#[test]
fn test_standard_concurr() {
    //erlang::compile(); //TODO wierd bug when running from uncompiled erlang
    erlang::run(&format!("tests/icfa_examples/concurr.erl"));
    let core = erlang::get_core(&format!("tests/icfa_examples/concurr.erl.json"));
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
    erlang::run(&format!("tests/icfa_examples/id.erl"));
    let core = erlang::get_core(&format!("tests/icfa_examples/id.erl.json"));
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
fn test_standard_rec_id() {
    //erlang::compile();
    erlang::run(&format!("tests/icfa_examples/rec_id.erl"));
    let core = erlang::get_core(&format!("tests/icfa_examples/rec_id.erl.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let (_ps, _m, s) = analyzer.run();

    contains(P::Literal("a"), "X", &s.value, &ast_helper);
    contains(P::Literal("b"), "X", &s.value, &ast_helper);
}
