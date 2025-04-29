mod ast;
mod erlang;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    assert_eq!(args.len(), 2, "cargo run --release <file_path>.erl");

    erlang::compile();
    erlang::run(&args[1]);

    let core = erlang::get_core();
    let typed_core = ast::type_core(core);
    dbg!(typed_core);
}
