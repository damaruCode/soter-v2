use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::process::Command;

use serde_json;

//use crate::ast;

pub fn get_core(file: &String) -> serde_json::Value {
    compile();
    run(file);

    let json = File::open("core.json").expect("core.json could not be opened");
    let mut buf_reader = BufReader::new(json);
    let mut contents = String::new();
    buf_reader
        .read_to_string(&mut contents)
        .expect("core.json could not be read");

    //ast::deserialize(&contents)
    deserialize(&contents)
}

fn compile() {
    //erlc ecorej.erl
    let c = Command::new("erlc")
        .arg("erlang/ecorej.erl")
        .output()
        .expect("failed to compile erlang");

    println!("erlang_compile_status: {}", c.status);
    assert!(c.status.success());
}

fn run(file: &String) {
    //erl -noshell -s ecorej to_corej <file_path> -s init stop
    let r = Command::new("erl")
        .args([
            "-noshell", "-s", "ecorej", "to_corej", file, "-s", "init", "stop",
        ])
        .output()
        .expect("failed to run erlang");

    println!("erlang_run_status: {}", r.status);
    assert!(r.status.success());
}

fn deserialize(json: &String) -> serde_json::Value {
    serde_json::from_str(json).expect("input json could not be parsed into serde_json::Value enum")
}
