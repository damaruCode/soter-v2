use serde::{Deserialize, Serialize};

pub fn deserialize(json: &String) -> Module {
    let module: Module = serde_json::from_str(json).expect("Error: Deserialize failed");
    module
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Module {
    r#type: String,
    anno: String,
    args: ModuleArgs,
}

#[derive(Serialize, Deserialize, Debug)]
struct ModuleArgs {
    name: Literal,
    exports: Vec<String>,
    attrs: Vec<Vec<String>>,
    defs: Vec<Vec<String>>,
}

#[derive(Serialize, Deserialize, Debug)]
struct Literal {
    r#type: String,
    anno: String,
    args: LiteralArgs,
}

#[derive(Serialize, Deserialize, Debug)]
struct LiteralArgs {
    val: String,
}
