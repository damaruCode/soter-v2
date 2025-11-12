use soter_v2::abstraction::standard::StandardAbstraction;
use soter_v2::analyzer::Analyzer;
use soter_v2::ast;
use soter_v2::ast::TypedCore;
use soter_v2::erlang;
use soter_v2::state_space::Value;
use soter_v2::state_space::VarName;
use soter_v2::util::AstHelper;

#[test]
fn test_id() {
    erlang::run(&"icfa_examples/rec_id.erl".to_string());
    let core = erlang::get_core(&"icfa_examples/rec_id.erl.json".to_string());

    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let (proc_states, mailboxes, store) = analyzer.run();

    let mut is_contained = false;
    for (vaddr, values) in store.value.inner {
        if vaddr.var_name == VarName::Atom("Y".to_string()) {
            for value in values {
                match value {
                    Value::Closure(c) => {
                        let tc = ast_helper.get(c.prog_loc);
                        match tc {
                            TypedCore::Literal(l) => match *l.val.clone() {
                                TypedCore::String(erls) => {
                                    if erls.inner == "a" {
                                        is_contained = true;
                                        continue;
                                    }
                                }
                                _ => panic!(),
                            },
                            _ => panic!(),
                        }
                    }
                    _ => panic!(),
                }
            }
        }
    }

    if !is_contained {
        panic!();
    }
}
