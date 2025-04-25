use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::process::Command;

pub fn get_core() -> serde_json::Value {
    let json = File::open("core_test.json").expect("core.json could not be opened");
    let mut buf_reader = BufReader::new(json);
    let mut contents = String::new();
    buf_reader
        .read_to_string(&mut contents)
        .expect("core.json could not be read");

    serde_json::from_str(&contents)
        .expect("input json could not be parsed into serde_json::Value enum")
}

pub fn compile() {
    //erlc ecorej.erl
    let c = Command::new("erlc")
        .arg("erlang/ecorej.erl")
        .output()
        .expect("failed to compile erlang");

    println!("erlang_compile_status: {}", c.status);
    assert!(c.status.success());
}

pub fn run(file: &String) {
    //erl -noshell -s ecorej to_core <file_path> -s init stop
    let r = Command::new("erl")
        .args([
            "-noshell", "-s", "ecorej", "to_core", file, "-s", "init", "stop",
        ])
        .output()
        .expect("failed to run erlang");

    println!("erlang_run_status: {}", r.status);
    assert!(r.status.success());

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
