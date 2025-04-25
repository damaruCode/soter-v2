use serde::{Deserialize, Serialize};
use serde_json::Map;
use serde_json::Number;
use serde_json::Value;

#[derive(Serialize, Deserialize, Debug)]
pub enum TypedCore {
    //
    Null,
    Bool(bool),
    Number(Number),
    String(String),

    //
    Tupel(Tupel<TypedCore>),
    List(List<TypedCore>),

    //
    Literal(Literal),
    Module(Module),
    Var(Var),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct List<T> {
    inner: Vec<T>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Tupel<T> {
    frst: Box<T>,
    scnd: Box<T>,
}

//-record(c_alias, {anno=[] :: list(), var :: cerl:cerl(),
//		  pat :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Alias {
    anno: List<TypedCore>,
    var: Box<TypedCore>,
    pat: Box<TypedCore>,
}
//-record(c_apply, {anno=[] :: list(), op :: cerl:cerl(),
//		  args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Apply {
    anno: List<TypedCore>,
    op: Box<TypedCore>,
    args: List<TypedCore>,
}
//-record(c_binary, {anno=[] :: list(), segments :: [cerl:c_bitstr()]}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Binary {
    anno: List<TypedCore>,
    segments: List<BitStr>,
}
//-record(c_bitstr, {anno=[] :: list(), val :: cerl:cerl(),
//		   size :: cerl:cerl(),
//		   unit :: cerl:cerl(),
//		   type :: cerl:cerl(),
//		   flags :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct BitStr {
    anno: List<TypedCore>,
    val: Box<TypedCore>,
    size: Box<TypedCore>,
    unit: Box<TypedCore>,
    r#type: Box<TypedCore>,
    flags: Box<TypedCore>,
}
//-record(c_call, {anno=[] :: list(), module :: cerl:cerl(),
//		 name :: cerl:cerl(),
//		 args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Call {
    anno: List<TypedCore>,
    module: Box<TypedCore>,
    name: Box<TypedCore>,
    args: List<TypedCore>,
}
//-record(c_case, {anno=[] :: list(), arg :: cerl:cerl(),
//		 clauses :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Case {
    anno: List<TypedCore>,
    arg: Box<TypedCore>,
    clauses: List<TypedCore>,
}
//-record(c_catch, {anno=[] :: list(), body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Catch {
    anno: List<TypedCore>,
    body: Box<TypedCore>,
}
//-record(c_clause, {anno=[] :: list(), pats :: [cerl:cerl()],
//		   guard :: cerl:cerl(),
//		   body :: cerl:cerl() | any()}). % todo
#[derive(Serialize, Deserialize, Debug)]
pub struct Clause {
    anno: List<TypedCore>,
    pats: List<TypedCore>,
    guard: Box<TypedCore>,
    body: Box<TypedCore>,
}
//-record(c_cons, {anno=[] :: list(), hd :: cerl:cerl(),
//		 tl :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Cons {
    anno: List<TypedCore>,
    hd: Box<TypedCore>,
    tl: Box<TypedCore>,
}
//-record(c_fun, {anno=[] :: list(), vars :: [cerl:cerl()],
//		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Fun {
    anno: List<TypedCore>,
    vars: List<TypedCore>,
    body: Box<TypedCore>,
}
//-record(c_let, {anno=[] :: list(), vars :: [cerl:cerl()],
//		arg :: cerl:cerl(),
//		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Let {
    anno: List<TypedCore>,
    vars: List<TypedCore>,
    arg: Box<TypedCore>,
    body: Box<TypedCore>,
}
//-record(c_letrec, {anno=[] :: list(),
//       defs :: [{cerl:cerl(), cerl:cerl()}],
//		   body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct LetRec {
    anno: List<TypedCore>,
    defs: List<Tupel<TypedCore>>,
    body: Box<TypedCore>,
}
//-record(c_literal, {anno=[] :: list(), val :: any()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Literal {
    anno: List<TypedCore>,
    val: Box<TypedCore>,
}
//-record(c_map, {anno=[] :: list(),
//		     arg=#c_literal{val=#{}} :: cerl:c_var() | cerl:c_literal(),
//		     es :: [cerl:c_map_pair()],
//		     is_pat=false :: boolean()}).
#[derive(Serialize, Deserialize, Debug)]
// TODO arg, is_pat ???
pub struct CoreMap {
    anno: List<TypedCore>,
    arg: Literal,
    es: List<MapPair>,
    is_pat: bool,
}
//-record(c_map_pair, {anno=[] :: list(),
//	       op :: #c_literal{val::'assoc'} | #c_literal{val::'exact'},
//		     key :: any(),              % todo
//		     val :: any()}).            % todo
#[derive(Serialize, Deserialize, Debug)]
// TODO op ???
pub struct MapPair {
    op: Literal,
    key: Box<TypedCore>,
    val: Box<TypedCore>,
}
//-record(c_module, {anno=[] :: list(), name :: cerl:cerl(),
//		   exports :: [cerl:cerl()],
//		   attrs :: [{cerl:cerl(), cerl:cerl()}],
//		   defs :: [{cerl:cerl(), cerl:cerl()}]}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Module {
    anno: List<TypedCore>,
    name: Box<TypedCore>,
    exports: List<TypedCore>,
    attrs: List<Tupel<TypedCore>>,
    defs: List<Tupel<TypedCore>>,
}
//-record(c_opaque, {anno=[] :: list(), val :: any()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Opaque {
    anno: List<TypedCore>,
    val: Box<TypedCore>,
}
//-record(c_primop, {anno=[] :: list(), name :: cerl:cerl(),
//		   args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug)]
pub struct PrimOp {
    anno: List<TypedCore>,
    name: Box<TypedCore>,
    args: List<TypedCore>,
}
//-record(c_receive, {anno=[] :: list(), clauses :: [cerl:cerl()],
//		    timeout :: cerl:cerl(),
//		    action :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Receive {
    anno: List<TypedCore>,
    clauses: List<TypedCore>,
    timeout: Box<TypedCore>,
    action: Box<TypedCore>,
}
//-record(c_seq, {anno=[] :: list(), arg :: cerl:cerl() | any(), % todo
//		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Seq {
    anno: List<TypedCore>,
    arg: Box<TypedCore>,
    body: Box<TypedCore>,
}
//-record(c_try, {anno=[] :: list(), arg :: cerl:cerl(),
//		vars :: [cerl:cerl()],
//		body :: cerl:cerl(),
//		evars :: [cerl:cerl()],
//		handler :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Try {
    anno: List<TypedCore>,
    arg: Box<TypedCore>,
    vars: List<TypedCore>,
    body: Box<TypedCore>,
    evars: List<TypedCore>,
    handler: Box<TypedCore>,
}
//-record(c_tuple, {anno=[] :: list(), es :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug)]
// TODO naming ???
pub struct CoreTupel {
    anno: List<TypedCore>,
    es: List<TypedCore>,
}
//-record(c_values, {anno=[] :: list(), es :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Values {
    anno: List<TypedCore>,
    es: List<TypedCore>,
}
//-record(c_var, {anno=[] :: list(), name :: cerl:var_name()}).
#[derive(Serialize, Deserialize, Debug)]
pub struct Var {
    anno: List<TypedCore>,
    name: Box<TypedCore>,
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

fn type_tupel(tupel: Vec<Value>) -> Tupel<TypedCore> {
    assert!(tupel.len() == 2);
    Tupel {
        frst: Box::new(type_core(tupel.get(0).unwrap().clone())),
        scnd: Box::new(type_core(tupel.get(1).unwrap().clone())),
    }
}

fn type_array(vec: Vec<Value>) -> List<TypedCore> {
    let mut list = Vec::new();
    for val in vec {
        list.push(type_core(val));
    }
    List { inner: list }
}

fn type_array_of_tuple(vec: Vec<Value>) -> List<Tupel<TypedCore>> {
    let mut list = Vec::new();
    for val in vec {
        list.push(type_tupel(val.as_array().unwrap().to_vec()));
    }
    List { inner: list }
}

fn type_object(map: Map<String, Value>) -> TypedCore {
    match map.get("type").unwrap().as_str().unwrap() {
        "c_literal" => {
            let l = Literal {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                val: Box::new(type_core(map.get("val").unwrap().clone())),
            };
            return TypedCore::Literal(l);
        }
        "c_module" => {
            let m = Module {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().to_vec()),
                name: Box::new(type_core(map.get("name").unwrap().clone())),
                exports: type_array(map.get("exports").unwrap().as_array().unwrap().to_vec()),
                attrs: type_array_of_tuple(map.get("attrs").unwrap().as_array().unwrap().to_vec()),
                defs: type_array_of_tuple(map.get("defs").unwrap().as_array().unwrap().to_vec()),
            };
            return TypedCore::Module(m);
        }
        "c_var" => {
            let v = Var {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                name: Box::new(type_core(map.get("name").unwrap().clone())),
            };
            return TypedCore::Var(v);
        }
        _ => panic!("obj not impled"),
    };
}
