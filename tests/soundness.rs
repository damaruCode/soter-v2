use soter_v2::abstraction::standard::KAddr;
use soter_v2::abstraction::standard::StandardAbstraction;
use soter_v2::abstraction::standard::VAddr;
use soter_v2::analyzer::Analyzer;
use soter_v2::ast;
use soter_v2::ast::TypedCore;
use soter_v2::erlang;
use soter_v2::state_space::Mailboxes;
use soter_v2::state_space::Store;
use soter_v2::state_space::Value;
use soter_v2::state_space::VarName;
use soter_v2::util::AstHelper;

fn check_store(
    store: &Store<KAddr, VAddr>,
    ast_helper: AstHelper,
    var_name: &str,
    values: Vec<&str>,
) {
    for (vaddr, val) in &store.value.inner {
        if vaddr.var_name == VarName::Atom(var_name.to_string()) {
            for value in val {
                match value {
                    Value::Closure(c) => {
                        let tc = ast_helper.get(c.prog_loc);
                        match tc {
                            TypedCore::Literal(l) => match *l.val.clone() {
                                TypedCore::String(erls) => {
                                    if !values.contains(&erls.inner.as_str()) {
                                        panic!("\"{}\" contains \"{}\"", var_name, erls.inner);
                                    }
                                }
                                _ => panic!("{} is not a string", l),
                            },
                            _ => panic!("{} is not a literal", tc),
                        }
                    }
                    _ => panic!("{} is not a closure", value), // TODO in concurr this fails
                }
            }
        }
    }
}

fn check_mailboxes(mailboxes: Mailboxes<VAddr>) {
    panic!("{}", &format!("{:?}", mailboxes)); // TODO impl
}

#[test]
fn test_standard_concurr() {
    erlang::run(&format!("tests/icfa_examples/concurr.erl"));
    let core = erlang::get_core(&format!("tests/icfa_examples/concurr.erl.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let (_ps, m, s, _f) = analyzer.run();

    check_store(&s, ast_helper.clone(), "P", vec![]);

    check_mailboxes(m);
}

#[test]
fn test_standard_id() {
    erlang::run(&format!("tests/icfa_examples/id.erl"));
    let core = erlang::get_core(&format!("tests/icfa_examples/id.erl.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let (_ps, m, s, _f) = analyzer.run();

    check_store(&s, ast_helper.clone(), "Y", vec!["a"]);
    check_store(&s, ast_helper.clone(), "Z", vec!["a", "b"]);
    check_store(&s, ast_helper.clone(), "X", vec!["a", "b"]);

    check_mailboxes(m);
}

#[test]
fn test_standard_rec_id() {
    erlang::run(&format!("tests/icfa_examples/rec_id.erl"));
    let core = erlang::get_core(&format!("tests/icfa_examples/rec_id.erl.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let (_ps, m, s, _f) = analyzer.run();

    check_store(&s, ast_helper.clone(), "Y", vec!["a"]);
    check_store(&s, ast_helper.clone(), "Z", vec!["a", "b"]);
    check_store(&s, ast_helper.clone(), "X", vec!["a", "b"]);

    check_mailboxes(m);
}
