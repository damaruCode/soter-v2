use soter_v2::abstraction::standard::KAddr;
use soter_v2::abstraction::standard::StandardAbstraction;
use soter_v2::abstraction::standard::VAddr;
use soter_v2::analyzer::Analyzer;
use soter_v2::ast;
use soter_v2::ast::TypedCore;
use soter_v2::erlang;
use soter_v2::state_space::Mailboxes;
use soter_v2::state_space::Pid;
use soter_v2::state_space::ProcState;
use soter_v2::state_space::Store;
use soter_v2::state_space::Value;
use soter_v2::state_space::VarName;
use soter_v2::util::AstHelper;
use soter_v2::util::SetMap;

fn check_closure(
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
                    _ => panic!("{} is not a closure", value),
                }
            }
        }
    }
}

fn check_process(
    proc_states: &SetMap<Pid, ProcState<KAddr, VAddr>>,
    store: &Store<KAddr, VAddr>,
    ast_helper: AstHelper,
    var_name: &str,
) {
    for (vaddr, val) in &store.value.inner {
        if vaddr.var_name == VarName::Atom(var_name.to_string()) {
            for value in val {
                match value {
                    Value::Pid(p) => {
                        proc_states
                            .get(p)
                            .expect(format!("{} is not pointing to a proc_state", p).as_str());
                        let tc = ast_helper.get(p.prog_loc);
                        match tc {
                            TypedCore::Call(c) => match *c.name.clone() {
                                TypedCore::Literal(l) => match *l.val.clone() {
                                    TypedCore::String(erls) => {
                                        if !erls.inner.eq("spawn") {
                                            panic!("\"{}\" is not a spawn", erls);
                                        }
                                    }
                                    _ => panic!("{} is not a string", l),
                                },
                                _ => panic!("{} is not a literal", c),
                            },
                            _ => panic!("{} is not a call", tc),
                        }
                    }
                    _ => panic!("{} is not a pid", value),
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

    let (ps, _m, s) = analyzer.run();

    check_process(&ps, &s, ast_helper.clone(), "P");
    check_process(&ps, &s, ast_helper.clone(), "X");

    //check_mailboxes(m);
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

    let (_ps, _m, s) = analyzer.run();

    check_closure(&s, ast_helper.clone(), "Y", vec!["a"]);
    check_closure(&s, ast_helper.clone(), "Z", vec!["a", "b"]);
    check_closure(&s, ast_helper.clone(), "X", vec!["a", "b"]);

    //check_mailboxes(m);
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

    let (_ps, _m, s) = analyzer.run();

    check_closure(&s, ast_helper.clone(), "Y", vec!["a"]);
    check_closure(&s, ast_helper.clone(), "Z", vec!["a", "b"]);
    check_closure(&s, ast_helper.clone(), "X", vec!["a", "b"]);

    //check_mailboxes(m);
}
