pub mod ast;
mod erlang;

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    assert_eq!(args.len(), 2, "cargo run --release <file_path>.erl");
    let module = erlang::get_core(&args[1]);
    dbg!(module);
}
