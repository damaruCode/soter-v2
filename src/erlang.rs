use std::process::Command;

pub fn get_core(file: &String) {
    compile();
    run(file);
    deserialize();
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

fn deserialize() {
    // TODO
}
