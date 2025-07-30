pub mod helper;

use serde::{Deserialize, Serialize};
use serde_json::Map;
use serde_json::Number;
use serde_json::Value;

#[derive(Debug)]
pub struct ConversionError;

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub enum TypedCore {
    //
    AstTuple(AstTuple<TypedCore>),
    AstList(AstList<TypedCore>),

    //
    Null,
    Bool(bool),
    Number(Number),
    String(String),

    //
    Alias(Alias),
    Apply(Apply),
    Binary(Binary),
    BitStr(BitStr),
    Call(Call),
    Case(Case),
    Catch(Catch),
    Clause(Clause),
    Cons(Cons),
    Fun(Fun),
    Let(Let),
    LetRec(LetRec),
    Literal(Literal),
    LiteralMap(LiteralMap),
    MapPair(MapPair),
    Module(Module),
    Opaque(Opaque),
    PrimOp(PrimOp),
    Receive(Receive),
    Seq(Seq),
    Try(Try),
    Tuple(Tuple),
    Values(Values),
    Var(Var),
    VarMap(VarMap),
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct AstList<T> {
    pub inner: Vec<T>,
}
impl<T> AstList<T> {
    fn new() -> Self {
        AstList { inner: Vec::new() }
    }

    fn push(&mut self, value: T) {
        self.inner.push(value);
    }
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct AstTuple<T> {
    pub frst: Box<T>,
    pub scnd: Box<T>,
}

//-record(c_alias, {anno=[] :: list(), var :: cerl:cerl(),
//		  pat :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Alias {
    pub anno: AstList<TypedCore>,
    pub var: Box<TypedCore>,
    pub pat: Box<TypedCore>,
}
//-record(c_apply, {anno=[] :: list(), op :: cerl:cerl(),
//		  args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Apply {
    pub anno: AstList<TypedCore>,
    pub op: Box<TypedCore>,
    pub args: AstList<TypedCore>,
}
//-record(c_binary, {anno=[] :: list(), segments :: [cerl:c_bitstr()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Binary {
    pub anno: AstList<TypedCore>,
    pub segments: AstList<BitStr>,
}
//-record(c_bitstr, {anno=[] :: list(), val :: cerl:cerl(),
//		   size :: cerl:cerl(),
//		   unit :: cerl:cerl(),
//		   type :: cerl:cerl(),
//		   flags :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct BitStr {
    pub anno: AstList<TypedCore>,
    pub val: Box<TypedCore>,
    pub size: Box<TypedCore>,
    pub unit: Box<TypedCore>,
    pub r#type: Box<TypedCore>,
    pub flags: Box<TypedCore>,
}
//-record(c_call, {anno=[] :: list(), module :: cerl:cerl(),
//		 name :: cerl:cerl(),
//		 args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Call {
    pub anno: AstList<TypedCore>,
    pub module: Box<TypedCore>,
    pub name: Box<TypedCore>,
    pub args: AstList<TypedCore>,
}
//-record(c_case, {anno=[] :: list(), arg :: cerl:cerl(),
//		 clauses :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Case {
    pub anno: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub clauses: AstList<TypedCore>,
}
//-record(c_catch, {anno=[] :: list(), body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Catch {
    pub anno: AstList<TypedCore>,
    pub body: Box<TypedCore>,
}
//-record(c_clause, {anno=[] :: list(), pats :: [cerl:cerl()],
//		   guard :: cerl:cerl(),
//		   body :: cerl:cerl() | any()}). % todo
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Clause {
    pub anno: AstList<TypedCore>,
    pub pats: AstList<TypedCore>,
    pub guard: Box<TypedCore>,
    pub body: Box<TypedCore>,
}
//-record(c_cons, {anno=[] :: list(), hd :: cerl:cerl(),
//		 tl :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Cons {
    pub anno: AstList<TypedCore>,
    pub hd: Box<TypedCore>,
    pub tl: Box<TypedCore>,
}
//-record(c_fun, {anno=[] :: list(), vars :: [cerl:cerl()],
//		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Fun {
    pub anno: AstList<TypedCore>,
    pub vars: AstList<TypedCore>,
    pub body: Box<TypedCore>,
}
//-record(c_let, {anno=[] :: list(), vars :: [cerl:cerl()],
//		arg :: cerl:cerl(),
//		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Let {
    pub anno: AstList<TypedCore>,
    pub vars: AstList<Var>,
    pub arg: Box<TypedCore>,
    pub body: Box<TypedCore>,
}

//-record(c_letrec, {anno=[] :: list(),
//       defs :: [{cerl:cerl(), cerl:cerl()}],
//		   body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct LetRec {
    pub anno: AstList<TypedCore>,
    pub defs: AstList<AstTuple<TypedCore>>,
    pub body: Box<TypedCore>,
}
//-record(c_literal, {anno=[] :: list(), val :: any()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Literal {
    pub anno: AstList<TypedCore>,
    pub val: Box<TypedCore>,
}
//-record(c_map, {anno=[] :: list(),
//		     arg=#c_literal{val=#{}} | cerl:c_var() | cerl:c_literal(), // NOTE maybe this ???
//		     arg=#c_literal{val=#{}} :: cerl:c_var() | cerl:c_literal(),
//		     es :: [cerl:c_map_pair()],
//		     is_pat=false :: boolean()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct LiteralMap {
    pub anno: AstList<TypedCore>,
    pub arg: Literal,
    pub es: AstList<MapPair>,
    pub is_pat: bool,
}
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct VarMap {
    pub anno: AstList<TypedCore>,
    pub arg: Var,
    pub es: AstList<MapPair>,
    pub is_pat: bool,
}
//-record(c_map_pair, {anno=[] :: list(),
//	       op :: #c_literal{val::'assoc'} | #c_literal{val::'exact'},
//		     key :: any(),              % todo
//		     val :: any()}).            % todo
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct MapPair {
    pub anno: AstList<TypedCore>,
    pub op: Literal,
    pub key: Box<TypedCore>,
    pub val: Box<TypedCore>,
}
//-record(c_module, {anno=[] :: list(), name :: cerl:cerl(),
//		   exports :: [cerl:cerl()],
//		   attrs :: [{cerl:cerl(), cerl:cerl()}],
//		   defs :: [{cerl:cerl(), cerl:cerl()}]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Module {
    pub anno: AstList<TypedCore>,
    pub name: Box<TypedCore>,
    pub exports: AstList<TypedCore>,
    pub attrs: AstList<AstTuple<TypedCore>>,
    pub defs: AstList<AstTuple<TypedCore>>,
}
//-record(c_opaque, {anno=[] :: list(), val :: any()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Opaque {
    pub anno: AstList<TypedCore>,
    pub val: Box<TypedCore>,
}
//-record(c_primop, {anno=[] :: list(), name :: cerl:cerl(),
//		   args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct PrimOp {
    pub anno: AstList<TypedCore>,
    pub name: Box<TypedCore>,
    pub args: AstList<TypedCore>,
}
//-record(c_receive, {anno=[] :: list(), clauses :: [cerl:cerl()],
//		    timeout :: cerl:cerl(),
//		    action :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Receive {
    pub anno: AstList<TypedCore>,
    pub clauses: AstList<TypedCore>,
    pub timeout: Box<TypedCore>,
    pub action: Box<TypedCore>,
}
//-record(c_seq, {anno=[] :: list(), arg :: cerl:cerl() | any(), % todo
//		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Seq {
    pub anno: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub body: Box<TypedCore>,
}
//-record(c_try, {anno=[] :: list(), arg :: cerl:cerl(),
//		vars :: [cerl:cerl()],
//		body :: cerl:cerl(),
//		evars :: [cerl:cerl()],
//		handler :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Try {
    pub anno: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub vars: AstList<TypedCore>,
    pub body: Box<TypedCore>,
    pub evars: AstList<TypedCore>,
    pub handler: Box<TypedCore>,
}
//-record(c_tuple, {anno=[] :: list(), es :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Tuple {
    pub anno: AstList<TypedCore>,
    pub es: AstList<TypedCore>,
}
//-record(c_values, {anno=[] :: list(), es :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Values {
    pub anno: AstList<TypedCore>,
    pub es: AstList<TypedCore>,
}
//-record(c_var, {anno=[] :: list(), name :: cerl:var_name()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Var {
    pub anno: AstList<TypedCore>,
    pub name: VarInner,
}
impl From<&Value> for Var {
    fn from(value: &Value) -> Self {
        Var::deserialize(value).unwrap()
    }
}
impl From<Vec<Value>> for AstList<Var> {
    fn from(vv: Vec<Value>) -> Self {
        vv.iter() // turn it into an iterable
            /* and do a fold on it with return type Result<Vec<Var>, ConversionError> */
            .fold(
                AstList::new(), /* base case is Ok( ) on the empty list */
                |mut acc,  /* accumulator */
                 curr_val  /* current element of iterable */| {
                    // if the accumulator is Ok( ) until now, continue conversion
                    acc.push(Var::from(curr_val));
                    acc
                },
            )
    }
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub enum VarInner {
    String(String),
    Number(Number),
}
impl TryFrom<serde_json::Value> for VarInner {
    type Error = ConversionError;

    fn try_from(value: serde_json::Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(s) => Ok(VarInner::String(s.clone())),
            Value::Number(n) => Ok(VarInner::Number(n.clone())),
            _ => Err(ConversionError),
        }
    }
}

pub fn type_core(core: Value) -> TypedCore {
    match core {
        Value::Null => TypedCore::Null,
        Value::Bool(bool) => TypedCore::Bool(bool),
        Value::Number(number) => TypedCore::Number(number),
        Value::String(string) => TypedCore::String(string),
        Value::Array(vec) => TypedCore::AstList(type_array(vec)),
        Value::Object(map) => type_object(map),
    }
}

fn type_tuple(tuple: Vec<Value>) -> AstTuple<TypedCore> {
    assert!(tuple.len() == 2);
    AstTuple {
        frst: Box::new(type_core(tuple.get(0).unwrap().clone())),
        scnd: Box::new(type_core(tuple.get(1).unwrap().clone())),
    }
}

fn type_array(vec: Vec<Value>) -> AstList<TypedCore> {
    let mut list = Vec::new();
    for val in vec {
        list.push(type_core(val));
    }
    AstList { inner: list }
}

fn type_bool(value: Value) -> Option<bool> {
    value.as_bool()
}

fn type_bitstr(value: Value) -> Result<BitStr, serde_json::Error> {
    BitStr::deserialize(value)
}

fn type_literal(value: Value) -> Result<Literal, serde_json::Error> {
    Literal::deserialize(value)
}

fn type_mappair(value: Value) -> Result<MapPair, serde_json::Error> {
    MapPair::deserialize(value)
}

fn type_var(value: Value) -> Result<Var, serde_json::Error> {
    Var::deserialize(value)
}

fn type_array_of_tuple(vec: Vec<Value>) -> AstList<AstTuple<TypedCore>> {
    let mut list = Vec::new();
    for val in vec {
        list.push(type_tuple(val.as_array().unwrap().to_vec()));
    }
    AstList { inner: list }
}

fn type_array_of_bitstr(vec: Vec<Value>) -> AstList<BitStr> {
    let mut list = Vec::new();
    for val in vec {
        list.push(type_bitstr(val).unwrap());
    }
    AstList { inner: list }
}

fn type_array_of_mappair(vec: Vec<Value>) -> AstList<MapPair> {
    let mut list = Vec::new();
    for val in vec {
        list.push(type_mappair(val).unwrap());
    }
    AstList { inner: list }
}

fn type_object(map: Map<String, Value>) -> TypedCore {
    match map.get("type").unwrap().as_str().unwrap() {
        "c_alias" => {
            let a = Alias {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                var: Box::new(type_core(map.get("var").unwrap().clone())),
                pat: Box::new(type_core(map.get("pat").unwrap().clone())),
            };
            return TypedCore::Alias(a);
        }
        "c_apply" => {
            let a = Apply {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                op: Box::new(type_core(map.get("op").unwrap().clone())),
                args: type_array(map.get("args").unwrap().as_array().unwrap().clone()),
            };
            return TypedCore::Apply(a);
        }
        "c_binary" => {
            let b = Binary {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                segments: type_array_of_bitstr(
                    map.get("segments").unwrap().as_array().unwrap().clone(),
                ),
            };
            return TypedCore::Binary(b);
        }
        "c_bitstr" => {
            let bs = BitStr {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                val: Box::new(type_core(map.get("val").unwrap().clone())),
                size: Box::new(type_core(map.get("size").unwrap().clone())),
                unit: Box::new(type_core(map.get("unit").unwrap().clone())),
                r#type: Box::new(type_core(map.get("type").unwrap().clone())),
                flags: Box::new(type_core(map.get("flags").unwrap().clone())),
            };
            return TypedCore::BitStr(bs);
        }
        "c_call" => {
            let c = Call {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                module: Box::new(type_core(map.get("module").unwrap().clone())),
                name: Box::new(type_core(map.get("name").unwrap().clone())),
                args: type_array(map.get("args").unwrap().as_array().unwrap().clone()),
            };
            return TypedCore::Call(c);
        }
        "c_case" => {
            let c = Case {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                arg: Box::new(type_core(map.get("arg").unwrap().clone())),
                clauses: type_array(map.get("clauses").unwrap().as_array().unwrap().clone()),
            };
            return TypedCore::Case(c);
        }
        "c_catch" => {
            let c = Catch {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                body: Box::new(type_core(map.get("body").unwrap().clone())),
            };
            return TypedCore::Catch(c);
        }
        "c_clause" => {
            let c = Clause {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                pats: type_array(map.get("pats").unwrap().as_array().unwrap().clone()),
                body: Box::new(type_core(map.get("body").unwrap().clone())),
                guard: Box::new(type_core(map.get("guard").unwrap().clone())),
            };
            return TypedCore::Clause(c);
        }
        "c_cons" => {
            let c = Cons {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                hd: Box::new(type_core(map.get("hd").unwrap().clone())),
                tl: Box::new(type_core(map.get("tl").unwrap().clone())),
            };
            return TypedCore::Cons(c);
        }
        "c_fun" => {
            let f = Fun {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                vars: type_array(map.get("vars").unwrap().as_array().unwrap().clone()),
                body: Box::new(type_core(map.get("body").unwrap().clone())),
            };
            return TypedCore::Fun(f);
        }
        "c_let" => {
            let l = Let {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                vars: map.get("vars").unwrap().as_array().unwrap().clone().into(),
                arg: Box::new(type_core(map.get("arg").unwrap().clone())),
                body: Box::new(type_core(map.get("body").unwrap().clone())),
            };
            return TypedCore::Let(l);
        }
        "c_letrec" => {
            let lr = LetRec {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                defs: type_array_of_tuple(map.get("defs").unwrap().as_array().unwrap().clone()),
                body: Box::new(type_core(map.get("body").unwrap().clone())),
            };
            return TypedCore::LetRec(lr);
        }
        "c_literal" => {
            let l = Literal {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                val: Box::new(type_core(map.get("val").unwrap().clone())),
            };
            return TypedCore::Literal(l);
        }
        "c_map" => {
            let lit = type_literal(map.get("arg").unwrap().clone());
            match lit {
                Ok(lit) => {
                    let lm = LiteralMap {
                        anno: type_array(map.get("anno").unwrap().as_array().unwrap().to_vec()),
                        arg: lit,
                        es: type_array_of_mappair(
                            map.get("es").unwrap().as_array().unwrap().to_vec(),
                        ),
                        is_pat: type_bool(map.get("is_pat").unwrap().clone()).unwrap(),
                    };
                    return TypedCore::LiteralMap(lm);
                }
                Err(_) => {
                    let vm = VarMap {
                        anno: type_array(map.get("anno").unwrap().as_array().unwrap().to_vec()),
                        arg: type_var(map.get("arg").unwrap().clone()).unwrap(),
                        es: type_array_of_mappair(
                            map.get("es").unwrap().as_array().unwrap().to_vec(),
                        ),
                        is_pat: type_bool(map.get("is_pat").unwrap().clone()).unwrap(),
                    };
                    return TypedCore::VarMap(vm);
                }
            }
        }
        "c_map_pair" => {
            let m = MapPair {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().to_vec()),
                op: type_literal(map.get("op").unwrap().clone()).unwrap(),
                key: Box::new(type_core(map.get("key").unwrap().clone())),
                val: Box::new(type_core(map.get("val").unwrap().clone())),
            };
            return TypedCore::MapPair(m);
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
        "c_opaque" => {
            let o = Opaque {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                val: Box::new(type_core(map.get("val").unwrap().clone())),
            };
            return TypedCore::Opaque(o);
        }
        "c_primop" => {
            let p = PrimOp {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                name: Box::new(type_core(map.get("name").unwrap().clone())),
                args: type_array(map.get("args").unwrap().as_array().unwrap().clone()),
            };
            return TypedCore::PrimOp(p);
        }
        "c_receive" => {
            let r = Receive {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                clauses: type_array(map.get("clauses").unwrap().as_array().unwrap().clone()),
                timeout: Box::new(type_core(map.get("timeout").unwrap().clone())),
                action: Box::new(type_core(map.get("action").unwrap().clone())),
            };
            return TypedCore::Receive(r);
        }
        "c_seq" => {
            let t = Seq {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                arg: Box::new(type_core(map.get("arg").unwrap().clone())),
                body: Box::new(type_core(map.get("body").unwrap().clone())),
            };
            return TypedCore::Seq(t);
        }
        "c_try" => {
            let t = Try {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                arg: Box::new(type_core(map.get("arg").unwrap().clone())),
                vars: type_array(map.get("vars").unwrap().as_array().unwrap().clone()),
                body: Box::new(type_core(map.get("body").unwrap().clone())),
                evars: type_array(map.get("evars").unwrap().as_array().unwrap().clone()),
                handler: Box::new(type_core(map.get("handler").unwrap().clone())),
            };
            return TypedCore::Try(t);
        }
        "c_tuple" => {
            let ct = Tuple {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                es: type_array(map.get("es").unwrap().as_array().unwrap().clone()),
            };
            return TypedCore::Tuple(ct);
        }
        "c_values" => {
            let v = Values {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                es: type_array(map.get("es").unwrap().as_array().unwrap().clone()),
            };
            return TypedCore::Values(v);
        }
        "c_var" => {
            let v = Var {
                anno: type_array(map.get("anno").unwrap().as_array().unwrap().clone()),
                name: match VarInner::try_from(map.get("name").unwrap().clone()) {
                    Ok(v) => v,
                    Err(e) => panic!("{:?}", e),
                },
            };
            return TypedCore::Var(v);
        }
        type_name => panic!("{} not impled", type_name),
    };
}
