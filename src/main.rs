pub mod abstraction;
pub mod analyzer;
pub mod ast;
pub mod erlang;
pub mod state_space;
pub mod util;

use std::{
    fs::{self, File},
    process,
    time::Instant,
};

use abstraction::{standard::StandardAbstraction, Abstraction, AbstractionKind};
use analyzer::Analyzer;
use chrono::Utc;
use clap::Parser;
use log4rs::{
    append::file::FileAppender,
    config::{Appender, Root},
    encode::pattern::PatternEncoder,
    Config,
};
use state_space::{FailureType, KontinuationAddress, ProgLocOrPid, ValueAddress};
use std::io::Write;
use util::{peek_print, AstHelper, EdgeAttributes, NodeAttributes};

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
    }
}

fn run_analysis_with<K: KontinuationAddress, V: ValueAddress>(
    abstraction: Box<dyn Abstraction<K, V>>,
    ast_helper: AstHelper,
    args: Cli,
) {
    let mut analyzer = Analyzer::new(ast_helper.clone(), abstraction);

    // Timing
    let seen;
    let mailboxes;
    let store;
    if args.stop_time {
        let instance = Instant::now();
        // Run
        (seen, mailboxes, store) = analyzer.run();

        let execution_time = instance.elapsed().as_nanos();
        let mut sum_states = 0;
        for (_, states) in &seen.inner {
            sum_states = sum_states + states.len();
        }
        println!("Time: {}, States: {}", execution_time, sum_states);
    } else {
        (seen, mailboxes, store) = analyzer.run();
    }

    // Printing Graph and logging output
    if args.export_graph {
        // graphs/<FILE_NAME>/
        let mut graph_dir = args.output_dir.join("graphs");
        graph_dir.push(args.file.file_stem().unwrap());

        if !graph_dir.is_dir() {
            fs::create_dir_all(&graph_dir).unwrap();
        }

        let graph_path = graph_dir.join(
            args.file
                .with_extension(format!(
                    "{}.{}.erl.dot",
                    match args.abstraction {
                        AbstractionKind::Standard => "standard",
                    },
                    args.time_depth
                ))
                .file_name()
                .unwrap()
                .to_os_string()
                .into_string()
                .unwrap(),
        );

        let dot_graph = analyzer.get_transition_graph().print_dot(
            // &graph_path.into_os_string().into_string().unwrap().as_str(),
            |proc_state| {
                let mut node_attr = NodeAttributes::new();
                node_attr.label = match &proc_state.prog_loc_or_pid {
                    ProgLocOrPid::Pid(pid) => format!("{}", pid),
                    ProgLocOrPid::ProgLoc(prog_loc) => {
                        let tc = ast_helper.get(*prog_loc);
                        peek_print::print(tc)
                    }
                };
                node_attr.tooltip = match &proc_state.failure_type {
                    FailureType::None => "".to_string(),
                    FailureType::Erlang(msg)
                    | FailureType::Unexpected(msg)
                    | FailureType::NotImplemented(msg) => msg.escape_default().to_string(),
                };
                // node_attr.tooltip = format!(
                //     "{}, {}, {}, {}, {}",
                //     proc_state.pid,
                //     proc_state.prog_loc_or_pid,
                //     proc_state.env,
                //     proc_state.k_addr,
                //     proc_state.time,
                // );

                node_attr.fill_color = match &proc_state.failure_type {
                    FailureType::None => "white".to_string(),
                    FailureType::NotImplemented(_) => "yellow".to_string(),
                    _ => "red".to_string(),
                };

                node_attr.group = format!("{}", proc_state.pid);

                node_attr
            },
            |transtion_name| {
                let mut edge_attr = EdgeAttributes::new();

                if !transtion_name.ends_with("revisit") {
                    edge_attr.label = transtion_name.clone();
                }

                edge_attr
            },
        );
        let mut graph_file = File::create(&graph_path).unwrap();
        write!(graph_file, "{}", dot_graph).unwrap();

        process::Command::new("sh")
            .arg("-c")
            .arg(format!(
                "unflatten -f -l3 -c6 {} | dot | neato -s -n2 -Tsvg > {}",
                graph_path.clone().into_os_string().into_string().unwrap(),
                graph_path
                    .with_extension("svg")
                    .into_os_string()
                    .into_string()
                    .unwrap()
            ))
            .output()
            .expect("Failed to compile dot file with graphviz");
    }

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
