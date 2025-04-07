use serde::{Deserialize, Serialize};
use serde_json::Map;
use serde_json::Value;

#[derive(Serialize, Deserialize, Debug)]
pub enum TypedCore {
    Null,
    Bool(bool),
    Number(serde_json::Number), // TODO maybe move away from serde here.
    String(String),
    List(Vec<TypedCore>),

    Literal(Literal),
    Module(Module),
}

//-record(c_literal,
//      {anno=[] :: list(),
//      val :: any()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Literal {
    anno: Box<TypedCore>,
    val: Box<TypedCore>,
}

//-record(c_module,
//      {anno=[] :: list(),
//      name :: cerl:cerl(),
//      exports :: [cerl:cerl()],
//	    attrs :: [{cerl:cerl(), cerl:cerl()}],
//		  defs :: [{cerl:cerl(), cerl:cerl()}]}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Module {
    anno: Vec<Typed>,
    name: Box<TypedCore>,
    exports: Vec<TypedCore>,
    attrs: Vec<TypedCore>, // TODO maybe add a tuple type for attrs and defs
    defs: Vec<TypedCore>,
}

pub fn type_core(core: Value) -> TypedCore {
    match core {
        Value::Null => TypedCore::Null,
        Value::Bool(bool) => TypedCore::Bool(bool),
        Value::Number(number) => TypedCore::Number(number),
        Value::String(string) => TypedCore::String(string),
        Value::Array(vec) => TypedCore::List(type_array(vec)),
        Value::Object(map) => type_object(map),
    }
}

fn type_array(vec: Vec<Value>) -> Vec<TypedCore> {
    let mut list = Vec::new();
    for val in vec {
        list.push(type_core(val));
    }
    list
}

fn type_object(map: Map<String, Value>) -> TypedCore {
    match map.get("type").unwrap().as_str().unwrap() {
        "c_literal" => {
            let l = Literal {
                anno: Box::new(type_core(map.get("anno").unwrap().clone())),
                val: Box::new(type_core(map.get("val").unwrap().clone())),
            };
            return TypedCore::Literal(l);
        }
        "c_module" => {
            let m = Module {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().to_vec()),
                name: Box::new(type_core(map.get("name").unwrap().clone())),
                exports: type_array(map.get("exports").unwrap().as_array().unwrap().to_vec()),
                attrs: type_array(map.get("attrs").unwrap().as_array().unwrap().to_vec()),
                defs: type_array(map.get("defs").unwrap().as_array().unwrap().to_vec()),
            };
            return TypedCore::Module(m);
        }
        _ => panic!("obj not impled"),
    };
}
