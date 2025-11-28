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
use soter_v2::ast::TypedCore;
use soter_v2::ast::Var;
use soter_v2::erlang;
use soter_v2::state_space::Value;
use soter_v2::state_space::VarName;
use soter_v2::util::AstHelper;
use soter_v2::util::SetMap;

enum P<'p> {
    Var,
    Literal(&'p str),
    List(Vec<P<'p>>),  // TypedCore::AstList
    Tuple(Vec<P<'p>>), // TypedCore::Tuple
}

fn equals() -> bool {
    let test = P::List(vec![P::Literal("a"), P::Tuple(vec![P::Var, P::Var])]); // {"a",(X,Y)}
    true
}

fn contains(
    clause: Clause,
    vaddr: &VAddr,
    val_store: &SetMap<VAddr, Value<VAddr>>,
    ast_helper: &AstHelper,
) -> bool {
    let cvec = vec![clause];
    let sub = MatchHelper::vmatch(&cvec, vaddr, val_store, ast_helper);
    for (_val, vec) in sub {
        if !vec.is_empty() {
            return true;
        }
    }
    false
}

fn contains_list() {}

fn contains_tuple() {}
fn contains_var() {}

fn contains_literal(
    literal: &str,
    var_name: &str,
    val_store: &SetMap<VAddr, Value<VAddr>>,
    ast_helper: &AstHelper,
) {
    let pattern = AstList::from(vec![TypedCore::Literal(Literal {
        anno: AstList::new(),
        val: Box::new(TypedCore::String(ErlString {
            inner: String::from(literal),
            index: MaybeIndex::None,
        })),
        index: MaybeIndex::None,
    })]);

    let clause = Clause {
        anno: AstList::new(),
        pats: pattern,
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

    for (vaddr, _val) in &val_store.inner {
        if vaddr.var_name == VarName::Atom(var_name.to_string()) {
            if contains(clause.clone(), vaddr, &val_store, &ast_helper) {
                return;
            }
        }
    }
    panic!();
}

#[test]
fn test_standard_concurr() {
    erlang::compile();
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
    erlang::compile();
    erlang::run(&format!("tests/icfa_examples/id.erl"));
    let core = erlang::get_core(&format!("tests/icfa_examples/id.erl.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let (_ps, _m, s) = analyzer.run();

    contains_literal("a", "X", &s.value, &ast_helper);
    contains_literal("b", "X", &s.value, &ast_helper);
}

#[test]
fn test_standard_rec_id() {
    erlang::compile();
    erlang::run(&format!("tests/icfa_examples/rec_id.erl"));
    let core = erlang::get_core(&format!("tests/icfa_examples/rec_id.erl.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let (_ps, _m, _s) = analyzer.run();
}
