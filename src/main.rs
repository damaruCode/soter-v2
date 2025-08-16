pub mod abstraction;
pub mod analyzer;
pub mod ast;
pub mod erlang;
pub mod state_space;
pub mod util;

use std::env;

use abstraction::StandardAbstraction;
use analyzer::Analyzer;
use chrono::Utc;
use log4rs::{
    append::file::FileAppender,
    config::{Appender, Root},
    encode::pattern::PatternEncoder,
    Config,
};

fn main() {
    // Logging
    let now = Utc::now();
    let logfile_path = format!("logs/{}.log", now.format("%Y-%m-%d_%H-%M-%S").to_string());

    let logfile = FileAppender::builder()
        .encoder(Box::new(PatternEncoder::new("{l} - {m}\n")))
        .build(logfile_path)
        .unwrap();
    let config = Config::builder()
        .appender(Appender::builder().build("logfile", Box::new(logfile)))
        .build(
            Root::builder()
                .appender("logfile")
                .build(log::LevelFilter::Debug),
        )
        .unwrap();
    log4rs::init_config(config).unwrap();

    // Main Logic
    let args: Vec<String> = env::args().collect();
    assert_eq!(args.len(), 2, "cargo run --release <file_path>.erl");

    erlang::compile();
    erlang::run(&args[1]);

    let mut core_path = args[1].to_string();
    core_path.push_str(".json");
    let core = erlang::get_core(&core_path);
    let typed_core = ast::TypedCore::from(core);

    let mut ast_helper = util::AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);
    log::debug!("{:#?}", indexed_typed_core);

    ast_helper.build_lookup(&indexed_typed_core);

    // Analysis

    let mut standard_analyzer =
        Analyzer::new(ast_helper.clone(), Box::new(StandardAbstraction::new(0)));

    let mut non_standard_analyzer =
        Analyzer::new(ast_helper, Box::new(StandardAbstraction::new(100)));

    let seen_one = standard_analyzer.run();
    let seen_two = non_standard_analyzer.run();

    // Eval
    for (pid, states) in seen_one.inner {
        let mt = Vec::new();
        let states_two = seen_two.get(&pid).unwrap_or(&mt);
        log::debug!(
            "{:#?} ONE {:#?} - TWO {:#?}",
            pid,
            states.len(),
            states_two.len()
        )
    }
}

#[cfg(test)]
mod benchmarks {
    use crate::abstraction::StandardAbstraction;
    use crate::analyzer::Analyzer;
    use crate::ast;
    use crate::erlang;
    use crate::util::AstHelper;

    fn run_and_check(erl_file: &str) {
        erlang::run(&format!("test/{}.erl", erl_file));
        let core = erlang::get_core(&format!("test/{}.erl.json", erl_file));
        let typed_core = ast::TypedCore::from(core);
        let mut ast_helper = AstHelper::new();
        let indexed_typed_core = ast_helper.build_indecies(typed_core);
        ast_helper.build_lookup(&indexed_typed_core);
        let mut analyzer = Analyzer::new(ast_helper, Box::new(StandardAbstraction::new(0)));
        analyzer.run();
    }

    #[test]
    fn test_bigring() {
        run_and_check("bigring");
    }

    #[test]
    fn test_concdb() {
        run_and_check("concdb");
    }

    #[test]
    fn test_dynlockb() {
        run_and_check("dynlockb");
    }

    #[test]
    fn test_finite_leader() {
        run_and_check("finite_leader");
    }

    #[test]
    fn test_finite_leader2() {
        run_and_check("finite_leader2");
    }

    #[test]
    fn test_firewall() {
        run_and_check("firewall");
    }

    #[test]
    fn test_howait() {
        run_and_check("howait");
    }

    #[test]
    fn test_huch() {
        run_and_check("huch");
    }

    #[test]
    fn test_lockb() {
        run_and_check("lockb");
    }

    #[test]
    fn test_luke() {
        run_and_check("luke");
    }

    #[test]
    fn test_match() {
        run_and_check("match");
    }

    #[test]
    fn test_match2() {
        run_and_check("match2");
    }

    #[test]
    fn test_match2_eeehhh() {
        run_and_check("match2_eeehhh");
    }

    #[test]
    fn test_parikh() {
        run_and_check("parikh");
    }

    #[test]
    fn test_pipe() {
        run_and_check("pipe");
    }

    #[test]
    fn test_race() {
        run_and_check("race");
    }

    #[test]
    fn test_reslockbeh() {
        run_and_check("reslockbeh");
    }

    #[test]
    fn test_ring() {
        run_and_check("ring");
    }

    #[test]
    fn test_safe_send() {
        run_and_check("safe_send");
    }

    #[test]
    fn test_scalaris() {
        run_and_check("scalaris");
    }

    #[test]
    fn test_sieve() {
        run_and_check("sieve");
    }

    #[test]
    fn test_sieve2() {
        run_and_check("sieve2");
    }

    #[test]
    fn test_simple_leader() {
        run_and_check("simple_leader");
    }

    #[test]
    fn test_soter() {
        run_and_check("soter");
    }

    #[test]
    fn test_state_factory() {
        run_and_check("state_factory");
    }

    #[test]
    fn test_stutter() {
        run_and_check("stutter");
    }

    #[test]
    fn test_test() {
        run_and_check("test");
    }

    #[test]
    fn test_test_core() {
        run_and_check("test_core");
    }

    #[test]
    fn test_thesis() {
        run_and_check("thesis");
    }

    #[test]
    fn test_thesis2() {
        run_and_check("thesis2");
    }

    #[test]
    fn test_ufirewall() {
        run_and_check("ufirewall");
    }

    #[test]
    fn test_unsafe_send() {
        run_and_check("unsafe_send");
    }

    #[test]
    fn test_workers() {
        run_and_check("workers");
    }
}
