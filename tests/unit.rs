use std::collections::VecDeque;

use soter_v2::abstraction::standard::StandardAbstraction;
use soter_v2::abstraction::Abstraction;
use soter_v2::analyzer::MatchHelper;
use soter_v2::ast::*;
use soter_v2::state_space::{Closure, Env, ProcState, ProgLocOrPid, Store, Time, Value};
use soter_v2::util::AstHelper;

#[test]
pub fn test_ast_builder() {
    // destination var that we want to check
    let mut dest_var = Var::new();
    dest_var.name = Box::new(TypedCore::String(ErlString::new(String::from("D"))));

    // source var that is going to be the case.arg
    let mut source_var = Var::new();
    source_var.name = Box::new(TypedCore::String(ErlString::new(String::from("S"))));

    // TODO SOURCE = something,
    let mut let_var = Let::new();
    let_var.vars = AstList::new();
    let_var.vars.inner.push(TypedCore::Var(source_var.clone()));

    // case SOURCE of <DESTINATION> end
    let mut case = Case::new();
    {
        let mut literal_true = Literal::new();
        literal_true.val = Box::new(TypedCore::String(ErlString::new(String::from("true"))));

        case.arg = Box::new(TypedCore::Var(source_var.clone()));
        let mut clause = Clause::new();
        clause.pats = AstList::new();
        clause.pats.inner.push(TypedCore::Var(dest_var));
        clause.guard = Box::new(TypedCore::Literal(literal_true));

        case.clauses = AstList::new();
        case.clauses.inner.push(TypedCore::Clause(clause));
    }
    let_var.body = Box::new(TypedCore::Case(case.clone()));

    // build indices
    let mut ast_helper = AstHelper::new();
    let indexed_ast_tc = ast_helper.build_indecies(TypedCore::Let(let_var));
    ast_helper.build_lookup(&indexed_ast_tc);

    // find source var_id in indexed_case_tc
    if let TypedCore::Let(indexed_let_var) = &indexed_ast_tc {
        if let TypedCore::Var(indexed_source_var) = &indexed_let_var.vars.inner[0] {
            // some initial setup
            let abstraction = StandardAbstraction::new(10);
            let mut store = Store::init(abstraction.stop_kaddr());
            let proc_state = ProcState::init(abstraction.stop_kaddr());

            // build source_vaddr
            let source_vaddr = abstraction.new_vaddr(
                &proc_state,
                *indexed_source_var.var_id.unwrap(),
                &ProgLocOrPid::ProgLoc(0),
                &Env::init(),
                &Time {
                    inner: VecDeque::from([0]),
                },
            );

            let source_val = Value::Closure(Closure {
                prog_loc: 0,
                env: Env::init(),
            });
            store.value.push(source_vaddr.clone(), source_val.clone());

            if let TypedCore::Case(indexed_case) = &*indexed_let_var.body {
                let clause_substs = MatchHelper::cs_match_vaddr(
                    &Vec::from(&indexed_case.clauses),
                    &source_vaddr,
                    &store.value,
                    &ast_helper,
                );
                if let TypedCore::Clause(indexed_clause) = &indexed_case.clauses.inner[0] {
                    if let TypedCore::Var(indexed_dest_var) = &indexed_clause.pats.inner[0] {
                        // check that SOURCE flows into DESTINATION
                        for (_, substs) in &clause_substs {
                            for subst in substs {
                                match subst.inner.get(&indexed_dest_var.var_id.unwrap()).unwrap() {
                                    ValueAddressOrValue::ValueAddress(dest_vaddr) => {
                                        let dest_vals = store.value.get(&dest_vaddr).unwrap();

                                        for dest_val in dest_vals {
                                            if *dest_val == source_val {
                                                return;
                                            }
                                        }
                                    }
                                    ValueAddressOrValue::Value(dest_val) => {
                                        if *dest_val == source_val {
                                            return;
                                        }
                                    }
                                }
                            }
                        }
                        panic!("{:?}", clause_substs)
                    }
                    panic!("{:?}", indexed_clause.pats.inner[0])
                }
                panic!()
            }
            panic!()
        }
        panic!("{:?}", indexed_let_var);
    }

    panic!()
}
