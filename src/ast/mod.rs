pub mod alias;
pub mod apply;
pub mod ast_list;
pub mod ast_tuple;
pub mod binary;
pub mod bit_str;
pub mod call;
pub mod case;
pub mod catch;
pub mod clause;
pub mod cons;
pub mod fun;
pub mod r#let;
pub mod let_rec;
pub mod literal;
pub mod literal_map;
pub mod map_pair;
pub mod module;
pub mod opaque;
pub mod prim_op;
pub mod receive;
pub mod seq;
pub mod r#try;
pub mod tuple;
pub mod values;
pub mod var;
pub mod var_map;

use crate::ast::alias::*;
use crate::ast::apply::*;
use crate::ast::ast_list::*;
use crate::ast::ast_tuple::*;
use crate::ast::binary::*;
use crate::ast::bit_str::*;
use crate::ast::call::*;
use crate::ast::case::*;
use crate::ast::catch::*;
use crate::ast::clause::*;
use crate::ast::cons::*;
use crate::ast::fun::*;
use crate::ast::let_rec::*;
use crate::ast::literal::*;
use crate::ast::literal_map::*;
use crate::ast::map_pair::*;
use crate::ast::module::*;
use crate::ast::opaque::*;
use crate::ast::prim_op::*;
use crate::ast::r#let::*;
use crate::ast::r#try::*;
use crate::ast::receive::*;
use crate::ast::seq::*;
use crate::ast::tuple::*;
use crate::ast::values::*;
use crate::ast::var::*;
use crate::ast::var_map::*;

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

impl From<Value> for TypedCore {
    fn from(core: Value) -> Self {
        match core {
            Value::Null => TypedCore::Null,
            Value::Bool(bool) => TypedCore::Bool(bool),
            Value::Number(number) => TypedCore::Number(number),
            Value::String(string) => TypedCore::String(string),
            Value::Array(vec) => TypedCore::AstList(AstList::from(vec)),
            Value::Object(map) => TypedCore::from(map),
        }
    }
}

impl From<Map<String, Value>> for TypedCore {
    fn from(map: Map<String, Value>) -> TypedCore {
        match map.get("type").unwrap().as_str().unwrap() {
            "c_alias" => {
                let a = Alias {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    var: Box::new(TypedCore::from(map.get("var").unwrap().clone())),
                    pat: Box::new(TypedCore::from(map.get("pat").unwrap().clone())),
                };
                return TypedCore::Alias(a);
            }
            "c_apply" => {
                let a = Apply {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    op: Box::new(TypedCore::from(map.get("op").unwrap().clone())),
                    args: AstList::from(map.get("args").unwrap().as_array().unwrap().clone()),
                };
                return TypedCore::Apply(a);
            }
            "c_binary" => {
                let b = Binary {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    segments: AstList::<BitStr>::from(
                        map.get("segments").unwrap().as_array().unwrap().clone(),
                    ),
                };
                return TypedCore::Binary(b);
            }
            "c_bitstr" => {
                let bs = BitStr {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    val: Box::new(TypedCore::from(map.get("val").unwrap().clone())),
                    size: Box::new(TypedCore::from(map.get("size").unwrap().clone())),
                    unit: Box::new(TypedCore::from(map.get("unit").unwrap().clone())),
                    r#type: Box::new(TypedCore::from(map.get("type").unwrap().clone())),
                    flags: Box::new(TypedCore::from(map.get("flags").unwrap().clone())),
                };
                return TypedCore::BitStr(bs);
            }
            "c_call" => {
                let c = Call {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    module: Box::new(TypedCore::from(map.get("module").unwrap().clone())),
                    name: Box::new(TypedCore::from(map.get("name").unwrap().clone())),
                    args: AstList::from(map.get("args").unwrap().as_array().unwrap().clone()),
                };
                return TypedCore::Call(c);
            }
            "c_case" => {
                let c = Case {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
                    clauses: AstList::from(map.get("clauses").unwrap().as_array().unwrap().clone()),
                };
                return TypedCore::Case(c);
            }
            "c_catch" => {
                let c = Catch {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
                };
                return TypedCore::Catch(c);
            }
            "c_clause" => {
                let c = Clause {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    pats: AstList::from(map.get("pats").unwrap().as_array().unwrap().clone()),
                    body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
                    guard: Box::new(TypedCore::from(map.get("guard").unwrap().clone())),
                };
                return TypedCore::Clause(c);
            }
            "c_cons" => {
                let c = Cons {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    hd: Box::new(TypedCore::from(map.get("hd").unwrap().clone())),
                    tl: Box::new(TypedCore::from(map.get("tl").unwrap().clone())),
                };
                return TypedCore::Cons(c);
            }
            "c_fun" => {
                let f = Fun {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    vars: AstList::from(map.get("vars").unwrap().as_array().unwrap().clone()),
                    body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
                };
                return TypedCore::Fun(f);
            }
            "c_let" => {
                let l = Let {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    vars: map.get("vars").unwrap().as_array().unwrap().clone().into(),
                    arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
                    body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
                };
                return TypedCore::Let(l);
            }
            "c_letrec" => {
                let lr = LetRec {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    defs: AstList::<AstTuple<TypedCore>>::from(
                        map.get("defs").unwrap().as_array().unwrap().clone(),
                    ),
                    body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
                };
                return TypedCore::LetRec(lr);
            }
            "c_literal" => {
                let l = Literal {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    val: Box::new(TypedCore::from(map.get("val").unwrap().clone())),
                };
                return TypedCore::Literal(l);
            }
            "c_map" => {
                let lit = Literal::try_from(map.get("arg").unwrap().clone());
                match lit {
                    Ok(lit) => {
                        let lm = LiteralMap {
                            anno: AstList::from(
                                map.get("anno").unwrap().as_array().unwrap().to_vec(),
                            ),
                            arg: lit,
                            es: AstList::<MapPair>::from(
                                map.get("es").unwrap().as_array().unwrap().to_vec(),
                            ),
                            is_pat: map.get("is_pat").unwrap().as_bool().unwrap(),
                        };
                        return TypedCore::LiteralMap(lm);
                    }
                    Err(_) => {
                        let vm = VarMap {
                            anno: AstList::from(
                                map.get("anno").unwrap().as_array().unwrap().to_vec(),
                            ),
                            arg: Var::from(map.get("arg").unwrap().clone()),
                            es: AstList::<MapPair>::from(
                                map.get("es").unwrap().as_array().unwrap().to_vec(),
                            ),
                            is_pat: map.get("is_pat").unwrap().as_bool().unwrap(),
                        };
                        return TypedCore::VarMap(vm);
                    }
                }
            }
            "c_map_pair" => {
                let m = MapPair {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().to_vec()),
                    op: Literal::from(map.get("op").unwrap().clone()),
                    key: Box::new(TypedCore::from(map.get("key").unwrap().clone())),
                    val: Box::new(TypedCore::from(map.get("val").unwrap().clone())),
                };
                return TypedCore::MapPair(m);
            }
            "c_module" => {
                let m = Module {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().to_vec()),
                    name: Box::new(TypedCore::from(map.get("name").unwrap().clone())),
                    exports: AstList::from(
                        map.get("exports").unwrap().as_array().unwrap().to_vec(),
                    ),
                    attrs: AstList::<AstTuple<TypedCore>>::from(
                        map.get("attrs").unwrap().as_array().unwrap().to_vec(),
                    ),
                    defs: AstList::<AstTuple<TypedCore>>::from(
                        map.get("defs").unwrap().as_array().unwrap().to_vec(),
                    ),
                };
                return TypedCore::Module(m);
            }
            "c_opaque" => {
                let o = Opaque {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    val: Box::new(TypedCore::from(map.get("val").unwrap().clone())),
                };
                return TypedCore::Opaque(o);
            }
            "c_primop" => {
                let p = PrimOp {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    name: Box::new(TypedCore::from(map.get("name").unwrap().clone())),
                    args: AstList::from(map.get("args").unwrap().as_array().unwrap().clone()),
                };
                return TypedCore::PrimOp(p);
            }
            "c_receive" => {
                let r = Receive {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    clauses: AstList::from(map.get("clauses").unwrap().as_array().unwrap().clone()),
                    timeout: Box::new(TypedCore::from(map.get("timeout").unwrap().clone())),
                    action: Box::new(TypedCore::from(map.get("action").unwrap().clone())),
                };
                return TypedCore::Receive(r);
            }
            "c_seq" => {
                let t = Seq {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
                    body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
                };
                return TypedCore::Seq(t);
            }
            "c_try" => {
                let t = Try {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
                    vars: AstList::from(map.get("vars").unwrap().as_array().unwrap().clone()),
                    body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
                    evars: AstList::from(map.get("evars").unwrap().as_array().unwrap().clone()),
                    handler: Box::new(TypedCore::from(map.get("handler").unwrap().clone())),
                };
                return TypedCore::Try(t);
            }
            "c_tuple" => {
                let ct = Tuple {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    es: AstList::from(map.get("es").unwrap().as_array().unwrap().clone()),
                };
                return TypedCore::Tuple(ct);
            }
            "c_values" => {
                let v = Values {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
                    es: AstList::from(map.get("es").unwrap().as_array().unwrap().clone()),
                };
                return TypedCore::Values(v);
            }
            "c_var" => {
                let v = Var {
                    anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
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
}
