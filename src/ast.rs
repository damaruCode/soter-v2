use serde::{Deserialize, Serialize};
use serde_json::Map;
use serde_json::Number;
use serde_json::Value;

#[derive(Serialize, Deserialize, Debug)]
pub enum TypedCore {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    List(Vec<TypedCore>),
    Object(TypedObject),
}

#[derive(Serialize, Deserialize, Debug)]
pub enum TypedObject {
    Literal(Literal),
    Module(Module),
    Var(Var),
}

//-record(c_literal,
//      {anno=[] :: list(),
//      val :: any()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Literal {
    anno: Vec<TypedCore>,
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
    anno: Vec<TypedCore>,
    name: Box<TypedCore>,
    exports: Vec<TypedCore>,
    attrs: Vec<(TypedCore, TypedCore)>,
    defs: Vec<(TypedCore, TypedCore)>,
}
//-record(c_var, {anno=[] :: list(),
//      name :: cerl:var_name()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Var {
    anno: Vec<TypedCore>,
    name: Box<TypedCore>,
}

pub fn type_core(core: Value) -> TypedCore {
    match core {
        Value::Null => TypedCore::Null,
        Value::Bool(bool) => TypedCore::Bool(bool),
        Value::Number(number) => TypedCore::Number(number),
        Value::String(string) => TypedCore::String(string),
        Value::Array(vec) => TypedCore::List(type_array(vec)),
        Value::Object(map) => TypedCore::Object(type_object(map)),
    }
}

fn type_tupel(tupel: Vec<Value>) -> (TypedCore, TypedCore) {
    assert!(tupel.len() == 2);
    (
        type_core(tupel.get(0).unwrap().clone()),
        type_core(tupel.get(1).unwrap().clone()),
    )
}

fn type_array(vec: Vec<Value>) -> Vec<TypedCore> {
    let mut list = Vec::new();
    for val in vec {
        list.push(type_core(val));
    }
    list
}

fn type_array_of_tuple(vec: Vec<Value>) -> Vec<(TypedCore, TypedCore)> {
    let mut list = Vec::new();
    for val in vec {
        list.push(type_tupel(val.as_array().unwrap().to_vec()));
    }
    list
}

fn type_object(map: Map<String, Value>) -> TypedObject {
    match map.get("type").unwrap().as_str().unwrap() {
        "c_literal" => {
            let l = Literal {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                val: Box::new(type_core(map.get("val").unwrap().clone())),
            };
            return TypedObject::Literal(l);
        }
        "c_module" => {
            let m = Module {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().to_vec()),
                name: Box::new(type_core(map.get("name").unwrap().clone())),
                exports: type_array(map.get("exports").unwrap().as_array().unwrap().to_vec()),
                attrs: type_array_of_tuple(map.get("attrs").unwrap().as_array().unwrap().to_vec()),
                defs: type_array_of_tuple(map.get("defs").unwrap().as_array().unwrap().to_vec()),
            };
            return TypedObject::Module(m);
        }
        "c_var" => {
            let v = Var {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                name: Box::new(type_core(map.get("name").unwrap().clone())),
            };
            return TypedObject::Var(v);
        }
        _ => panic!("obj not impled"),
    };
}
