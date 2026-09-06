use soter_v2::abstraction::standard::StandardAbstraction;
use soter_v2::analyzer::Analyzer;
use soter_v2::ast;
use soter_v2::erlang;
use soter_v2::util::AstHelper;

fn run_and_analyze(erl_file: &str) {
    erlang::compile();
    erlang::run(&format!("tests/benchmarks/{erl_file}.erl"));
    let core = erlang::get_core(&format!("tests/benchmarks/{erl_file}.erl.json"));
    let typed_core = ast::TypedCore::from(core);
    let mut ast_helper = AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    ast_helper.build_lookup(&indexed_typed_core);
    let mut analyzer = Analyzer::new(ast_helper, Box::new(StandardAbstraction::new(0)));

    analyzer.run();
}

#[test]
fn test_bigring() {
    run_and_analyze("bigring");
}

#[test]
fn test_concdb() {
    run_and_analyze("concdb");
}

#[test]
fn test_dynlockb() {
    run_and_analyze("dynlockb");
}

#[test]
fn test_finite_leader() {
    run_and_analyze("finite_leader");
}

#[test]
fn test_finite_leader2() {
    run_and_analyze("finite_leader2");
}

#[test]
fn test_firewall() {
    run_and_analyze("firewall");
}

#[test]
fn test_howait() {
    run_and_analyze("howait");
}

#[test]
fn test_huch() {
    run_and_analyze("huch");
}

#[test]
fn test_lockb() {
    run_and_analyze("lockb");
}

#[test]
fn test_luke() {
    run_and_analyze("luke");
}

#[test]
fn test_match() {
    run_and_analyze("match");
}

#[test]
fn test_match2() {
    run_and_analyze("match2");
}

#[test]
fn test_match2_eeehhh() {
    run_and_analyze("match2_eeehhh");
}

#[test]
fn test_parikh() {
    run_and_analyze("parikh");
}

#[test]
fn test_pipe() {
    run_and_analyze("pipe");
}

#[test]
fn test_race() {
    run_and_analyze("race");
}

#[test]
fn test_reslockbeh() {
    run_and_analyze("reslockbeh");
}

#[test]
fn test_ring() {
    run_and_analyze("ring");
}

#[test]
fn test_safe_send() {
    run_and_analyze("safe_send");
}

#[test]
fn test_scalaris() {
    run_and_analyze("scalaris");
}

#[test]
fn test_sieve() {
    run_and_analyze("sieve");
}

#[test]
fn test_sieve2() {
    run_and_analyze("sieve2");
}

#[test]
fn test_simple_leader() {
    run_and_analyze("simple_leader");
}

#[test]
fn test_soter() {
    run_and_analyze("soter");
}

#[test]
fn test_state_factory() {
    run_and_analyze("state_factory");
}

#[test]
fn test_stutter() {
    run_and_analyze("stutter");
}

#[test]
fn test_test() {
    run_and_analyze("test");
}

#[test]
fn test_test_core() {
    run_and_analyze("test_core");
}

#[test]
fn test_thesis() {
    run_and_analyze("thesis");
}

#[test]
fn test_thesis2() {
    run_and_analyze("thesis2");
}

#[test]
fn test_ufirewall() {
    run_and_analyze("ufirewall");
}

#[test]
fn test_unsafe_send() {
    run_and_analyze("unsafe_send");
}

#[test]
fn test_workers() {
    run_and_analyze("workers");
}
