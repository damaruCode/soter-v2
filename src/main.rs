pub mod abstraction;
pub mod analyzer;
pub mod ast;
pub mod erlang;
pub mod state_space;
pub mod util;

use std::{fs, time::Instant};

use abstraction::{
    icfa::ICFAAbstraction, p4f::P4FAbstraction, standard::StandardAbstraction,
    vp4f::VP4FAbstraction, Abstraction, AbstractionKind,
};
use analyzer::Analyzer;
use chrono::Utc;
use clap::Parser;
use log4rs::{
    append::file::FileAppender,
    config::{Appender, Root},
    encode::pattern::PatternEncoder,
    Config,
};
use state_space::{KontinuationAddress, ValueAddress};
use util::AstHelper;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Path to the Erlang file to analyze
    file: std::path::PathBuf,

    #[arg(short, long, default_value = "out")]
    output_dir: std::path::PathBuf,

    #[arg(short = 'l', long)]
    log: bool,

    #[arg(short = 'g', long)]
    export_graph: bool,

    #[arg(long)]
    stop_time: bool,

    #[arg(short = 'a', long)]
    abstraction: AbstractionKind,

    #[arg(short = 't', long)]
    time_depth: usize,
}

fn main() {
    // Arguments parsing
    let args = Cli::parse();

    // output directory
    if !args.output_dir.is_dir() {
        fs::create_dir_all(&args.output_dir).unwrap();
    }

    // Logging
    if args.log {
        let now = Utc::now();
        let logfile_path = args.output_dir.join(format!(
            "logs/{}.log",
            now.format("%Y-%m-%d_%H-%M-%S").to_string()
        ));

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
    }

    // Compiling to Core (JSON format)
    erlang::compile();
    erlang::run(&args.file.clone().into_os_string().into_string().unwrap());

    let core_path = args.file.with_extension("erl.json");
    let core = erlang::get_core(&core_path.into_os_string().into_string().unwrap());
    let typed_core = ast::TypedCore::from(core);

    // Indexing the AST
    let mut ast_helper = util::AstHelper::new();
    let indexed_typed_core = ast_helper.build_indecies(typed_core);

    ast_helper.build_lookup(&indexed_typed_core);

    match args.abstraction {
        AbstractionKind::Standard => run_analysis_with(
            Box::new(StandardAbstraction::new(args.time_depth)),
            ast_helper,
            args,
        ),
        AbstractionKind::P4F => run_analysis_with(
            Box::new(P4FAbstraction::new(args.time_depth)),
            ast_helper,
            args,
        ),
        AbstractionKind::VP4F => run_analysis_with(
            Box::new(VP4FAbstraction::new(args.time_depth)),
            ast_helper,
            args,
        ),
        AbstractionKind::ICFA => run_analysis_with(
            Box::new(ICFAAbstraction::new(args.time_depth)),
            ast_helper,
            args,
        ),
    }
}

fn run_analysis_with<K: KontinuationAddress, V: ValueAddress>(
    abstraction: Box<dyn Abstraction<K, V>>,
    ast_helper: AstHelper,
    args: Cli,
) {
    let mut graph_builder;
    if args.export_graph {
        graph_builder = util::graphviz::GraphBuilder::new();
    } else {
        graph_builder = util::graphviz::GraphBuilder::dummy();
    }

    let mut analyzer = Analyzer::new(ast_helper, abstraction);

    // Timing
    let seen;
    let mailboxes;
    let store;
    if args.stop_time {
        let instance = Instant::now();
        // Run
        (seen, mailboxes, store) = analyzer.run(&mut graph_builder);

        let execution_time = instance.elapsed().as_nanos();
        println!("Time: {} ns", execution_time);
    } else {
        (seen, mailboxes, store) = analyzer.run(&mut graph_builder);
    }

    // Printing Graph and logging output
    let mut graph_dir = args.output_dir.join("graphs");
    graph_dir.push(match args.abstraction {
        AbstractionKind::Standard => "standard",
        AbstractionKind::P4F => "p4f",
        AbstractionKind::VP4F => "vp4f",
        AbstractionKind::ICFA => "icfa",
    });
    if !graph_dir.is_dir() {
        fs::create_dir_all(&graph_dir).unwrap();
    }

    let graph_path = graph_dir.join(
        args.file
            .with_extension(format!("{}.erl.svg", args.time_depth))
            .file_name()
            .unwrap()
            .to_os_string()
            .into_string()
            .unwrap(),
    );
    graph_builder.print(&graph_path.into_os_string().into_string().unwrap());

    // Eval
    for (pid, states) in seen.inner {
        log::debug!("Seen: {}, {}", pid, states.len());
    }
    for (pid, mailbox) in mailboxes.inner {
        log::debug!("Mailbox: {}, {}", pid, mailbox);
    }
    log::debug!("KontStore:\n{}", store.kont);
    log::debug!("ValueStore:\n{}", store.value);
}

#[cfg(test)]
mod benchmarks {
    use crate::abstraction::standard::StandardAbstraction;
    use crate::analyzer::Analyzer;
    use crate::ast;
    use crate::erlang;
    use crate::util::graphviz::GraphBuilder;
    use crate::util::AstHelper;

    fn run_and_check(erl_file: &str) {
        erlang::run(&format!("test/{}.erl", erl_file));
        let core = erlang::get_core(&format!("test/{}.erl.json", erl_file));
        let typed_core = ast::TypedCore::from(core);
        let mut ast_helper = AstHelper::new();
        let indexed_typed_core = ast_helper.build_indecies(typed_core);
        ast_helper.build_lookup(&indexed_typed_core);
        let mut analyzer = Analyzer::new(ast_helper, Box::new(StandardAbstraction::new(0)));

        let mut graph_builder = GraphBuilder::new();
        analyzer.run(&mut graph_builder);
        graph_builder.print(&format!("test/{}.svg", erl_file));
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
