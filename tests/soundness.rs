use serde_json::Number;
use soter_v2::abstraction::standard::StandardAbstraction;
use soter_v2::abstraction::Abstraction;
use soter_v2::analyzer::Analyzer;
use soter_v2::analyzer::MatchHelper;
use soter_v2::ast;
use soter_v2::ast::AstList;
use soter_v2::ast::Clause;
use soter_v2::ast::Cons;
use soter_v2::ast::ErlBool;
use soter_v2::ast::ErlNumber;
use soter_v2::ast::ErlString;
use soter_v2::ast::Literal;
use soter_v2::ast::MaybeIndex;
use soter_v2::ast::Tuple;
use soter_v2::ast::TypedCore;
use soter_v2::ast::Var;
use soter_v2::erlang;
use soter_v2::state_space::KontinuationAddress;
use soter_v2::state_space::Pid;
use soter_v2::state_space::ProcState;
use soter_v2::state_space::ProgLocOrPid;
use soter_v2::state_space::Store;
use soter_v2::state_space::ValueAddress;
use soter_v2::state_space::VarName;
use soter_v2::util::AstHelper;
use soter_v2::util::SetMap;

#[derive(Debug)]
enum TestError {
    Lookup(String),
}

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
                val: Box::new(TypedCore::Bool(ErlBool::new(true))),
                index: MaybeIndex::None,
            })),
            body: Box::new(TypedCore::Dummy),
            index: MaybeIndex::None,
        };
        clause
    }
}

struct AnalyzerResult<K: KontinuationAddress, V: ValueAddress> {
    procs: SetMap<Pid, ProcState<K, V>>,
    store: Store<K, V>,
}

impl<K: KontinuationAddress, V: ValueAddress> AnalyzerResult<K, V> {
    fn contains(&self, var_name: &str, pattern: P, ast_helper: &AstHelper) {
        let cvec = vec![Clause::from(pattern.clone())];

        // 1. find the variable in the source code
        let vars = ast_helper
            .get_vars(&VarName::Atom(var_name.to_string()))
            .ok_or(TestError::Lookup(var_name.to_string()))
            .unwrap();

        // 2. find the variable in the proc states (by prog_loc)
        let mut vaddrs = Vec::new();
        for var in vars {
            for (_, procs) in self.procs.inner.iter() {
                for proc in procs {
                    if let ProgLocOrPid::ProgLoc(pl) = proc.prog_loc_or_pid {
                        if let MaybeIndex::Some(var_pl) = var.index {
                            if pl == var_pl {
                                if let Some(vaddr) = proc.env.inner.get(var.var_id.unwrap()) {
                                    vaddrs.push(vaddr);
                                }
                            }
                        }
                    }
                }
            }
        }

        // 3. match on each value bound to each vaddr

        for vaddr in vaddrs {
            let sub = MatchHelper::cs_match_vaddr(&cvec, vaddr, &self.store.value, &ast_helper);

            for (_val, vec) in sub {
                println!("subst val {} and vec {:#?}", _val, vec);
                if !vec.is_empty() {
                    return;
                }
            }
        }
        panic!("No entry {} matching {:#?} found.", var_name, pattern)
    }

    fn ncontains(&self, var_name: &str, pattern: P, ast_helper: &AstHelper) {
        let cvec = vec![Clause::from(pattern)];

        // 1. find the variable in the ast
        let vars = ast_helper
            .get_vars(&VarName::Atom(var_name.to_string()))
            .ok_or(TestError::Lookup(var_name.to_string()))
            .unwrap();

        // 2. find the variable in the proc states (by prog_loc)
        let mut vaddrs = Vec::new();
        for var in vars {
            for (_, procs) in self.procs.inner.iter() {
                for proc in procs {
                    if let ProgLocOrPid::ProgLoc(pl) = proc.prog_loc_or_pid {
                        if let MaybeIndex::Some(var_pl) = var.index {
                            if pl == var_pl {
                                if let Some(vaddr) = proc.env.inner.get(var.var_id.unwrap()) {
                                    vaddrs.push(vaddr);
                                }
                            }
                        }
                    }
                }
            }
        }

        // 3. match on each value bound to each vaddr
        for vaddr in vaddrs {
            let sub = MatchHelper::cs_match_vaddr(&cvec, vaddr, &self.store.value, ast_helper);

            for (_val, vec) in sub {
                if !vec.is_empty() {
                    panic!();
                }
            }
        }
    }
}

fn analyze_file<K, V, F>(filepath: &str, abstraction: Box<dyn Abstraction<K, V>>, checks: F)
where
    K: KontinuationAddress,
    V: ValueAddress,
    F: FnOnce(AnalyzerResult<K, V>, AstHelper) -> (),
{
    erlang::run(&filepath.to_string());
    let core = erlang::get_core(&format!("{filepath}.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);

    let mut analyzer = Analyzer::new(ast_helper.clone(), abstraction);
    let (procs, _, store) = analyzer.run();
    let res = AnalyzerResult { procs, store };

    checks(res, ast_helper);
}

#[test]
fn test_standard_id() {
    analyze_file(
        "tests/soundness/id.erl",
        Box::new(StandardAbstraction::new(0)),
        |res, ast_helper| {
            res.contains("X", P::Literal("a"), &ast_helper);
            res.contains("X", P::Literal("b"), &ast_helper);
            res.ncontains("X", P::Literal("c"), &ast_helper);
        },
    );
}

#[test]
fn test_standard_receive_lit() {
    //erlang::compile();
    analyze_file(
        "tests/soundness/receive_lit.erl",
        Box::new(StandardAbstraction::new(0)),
        |res, ast_helper| {
            res.contains("X", P::Literal("a"), &ast_helper);
            res.ncontains("X", P::Literal("M"), &ast_helper);
        },
    );
}

#[test]
fn test_standard_concurr() {
    analyze_file(
        "tests/soundness/concurr.erl",
        Box::new(StandardAbstraction::new(0)),
        |_, _| {},
    );
}

#[test]
fn test_standard_rec_id() {
    analyze_file(
        "tests/soundness/rec_id.erl",
        Box::new(StandardAbstraction::new(0)),
        |res, ast_helper| {
            res.contains("X", P::Literal("a"), &ast_helper);
            res.contains("X", P::Literal("b"), &ast_helper);
        },
    );
}

#[test]
fn test_pm_var_in_value() {
    analyze_file(
        "tests/soundness/pm_var_in_value.erl",
        Box::new(StandardAbstraction::new(0)),
        |res, ast_helper| {
            res.contains("R", P::Literal("b"), &ast_helper);
        },
    );
}
