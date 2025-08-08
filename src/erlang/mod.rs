use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::process::Command;

pub fn get_core(file: &String) -> serde_json::Value {
    let json = File::open(file).expect(&format!("{} could not be opened", file));
    let mut buf_reader = BufReader::new(json);
    let mut contents = String::new();
    buf_reader
        .read_to_string(&mut contents)
        .expect(&format!("{} could not be read", file));

    serde_json::from_str(&contents)
        .expect("input json could not be parsed into serde_json::Value enum")
}

pub fn compile() {
    //erlc -o ebin src/jsx*.erl
    let c = Command::new("erlc")
        .args(["-o", "erlang/ebin", "erlang/jsx/src/jsx*.erl"])
        .output()
        .expect("failed to compile jsx");

    println!("jsx_compile_status: {}", c.status);
    assert!(c.status.success());

    //erlc ecorej.erl
    let c = Command::new("erlc")
        .arg("erlang/ecorej.erl")
        .output()
        .expect("failed to compile ecorej");

    println!("ecorej_compile_status: {}", c.status);
    assert!(c.status.success());
}

pub fn run(file: &String) {
    //erl -noshell -s ecorej to_core <file_path> -s init stop
    let r = Command::new("erl")
        .args([
            "-pa",
            "erlang/ebin",
            "-noshell",
            "-s",
            "ecorej",
            "to_corej",
            file,
            "-s",
            "init",
            "stop",
        ])
        .output()
        .expect("failed to run erlang");

    println!("erlang_run_status: {}", r.status);
    assert!(r.status.success());
}
