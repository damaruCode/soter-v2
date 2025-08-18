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
pub mod erl_bool;
pub mod erl_map;
pub mod erl_null;
pub mod erl_number;
pub mod erl_string;
pub mod fun;
pub mod r#let;
pub mod let_rec;
pub mod literal;
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

use std::fmt::Display;

pub use alias::*;
pub use apply::*;
pub use ast_list::*;
pub use ast_tuple::*;
pub use binary::*;
pub use bit_str::*;
pub use call::*;
pub use case::*;
pub use catch::*;
pub use clause::*;
pub use cons::*;
pub use erl_bool::*;
pub use erl_map::*;
pub use erl_null::*;
pub use erl_number::*;
pub use erl_string::*;
pub use fun::*;
pub use let_rec::*;
pub use literal::*;
pub use map_pair::*;
pub use module::*;
pub use opaque::*;
pub use prim_op::*;
pub use r#let::*;
pub use r#try::*;
pub use receive::*;
pub use seq::*;
pub use tuple::*;
pub use values::*;
pub use var::*;

use serde::{Deserialize, Serialize};
use serde_json::Map;
use serde_json::Value;

#[derive(Debug)]
pub struct ConversionError;

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub enum TypedCore {
    //
    AstTuple(AstTuple<TypedCore>),
    AstList(AstList<TypedCore>),

    //
    Null(ErlNull),
    Bool(ErlBool),
    Number(ErlNumber),
    String(ErlString),

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
    Map(ErlMap),
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
}

pub trait Index {
    fn get_index(&self) -> Option<usize>;
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Hash, Clone)]
pub enum MaybeIndex {
    Some(usize),
    None,
}

impl From<MaybeIndex> for Option<usize> {
    fn from(value: MaybeIndex) -> Self {
        match value {
            MaybeIndex::Some(i) => Some(i),
            MaybeIndex::None => None,
        }
    }
}

impl Index for TypedCore {
    fn get_index(&self) -> Option<usize> {
        match self {
            TypedCore::AstTuple(ast) => ast.index.clone().into(),
            TypedCore::AstList(list) => list.index.clone().into(),

            TypedCore::Null(null) => null.index.clone().into(),
            TypedCore::Bool(bool) => bool.index.clone().into(),
            TypedCore::Number(number) => number.index.clone().into(),
            TypedCore::String(string) => string.index.clone().into(),

            TypedCore::Alias(alias) => alias.index.clone().into(),
            TypedCore::Apply(apply) => apply.index.clone().into(),
            TypedCore::Binary(binary) => binary.index.clone().into(),
            TypedCore::BitStr(bitstr) => bitstr.index.clone().into(),
            TypedCore::Call(call) => call.index.clone().into(),
            TypedCore::Case(case) => case.index.clone().into(),
            TypedCore::Catch(catch) => catch.index.clone().into(),
            TypedCore::Clause(clause) => clause.index.clone().into(),
            TypedCore::Cons(cons) => cons.index.clone().into(),
            TypedCore::Fun(fun) => fun.index.clone().into(),
            TypedCore::Let(let_) => let_.index.clone().into(),
            TypedCore::LetRec(letrec) => letrec.index.clone().into(),
            TypedCore::Literal(lit) => lit.index.clone().into(),
            TypedCore::Map(map) => map.index.clone().into(),
            TypedCore::MapPair(pair) => pair.index.clone().into(),
            TypedCore::Module(module) => module.index.clone().into(),
            TypedCore::Opaque(opq) => opq.index.clone().into(),
            TypedCore::PrimOp(prim) => prim.index.clone().into(),
            TypedCore::Receive(recv) => recv.index.clone().into(),
            TypedCore::Seq(seq) => seq.index.clone().into(),
            TypedCore::Try(try_) => try_.index.clone().into(),
            TypedCore::Tuple(tuple) => tuple.index.clone().into(),
            TypedCore::Values(vals) => vals.index.clone().into(),
            TypedCore::Var(var) => var.index.clone().into(),
        }
    }
}

impl From<Value> for TypedCore {
    fn from(core: Value) -> Self {
        match core {
            Value::Null => TypedCore::Null(ErlNull {
                index: MaybeIndex::None,
            }),
            Value::Bool(bool) => TypedCore::Bool(ErlBool {
                inner: bool,
                index: MaybeIndex::None,
            }),
            Value::Number(number) => TypedCore::Number(ErlNumber {
                inner: number,
                index: MaybeIndex::None,
            }),
            Value::String(string) => TypedCore::String(ErlString {
                inner: string,
                index: MaybeIndex::None,
            }),
            Value::Array(vec) => TypedCore::AstList(AstList::from(vec)),
            Value::Object(map) => TypedCore::from(map),
        }
    }
}

impl From<Map<String, Value>> for TypedCore {
    fn from(map: Map<String, Value>) -> TypedCore {
        match map.get("type").unwrap().as_str().unwrap() {
            "c_alias" => TypedCore::Alias(Alias::from(map)),
            "c_apply" => TypedCore::Apply(Apply::from(map)),
            "c_binary" => TypedCore::Binary(Binary::from(map)),
            "c_bitstr" => TypedCore::BitStr(BitStr::from(map)),
            "c_call" => TypedCore::Call(Call::from(map)),
            "c_case" => TypedCore::Case(Case::from(map)),
            "c_catch" => TypedCore::Catch(Catch::from(map)),
            "c_clause" => TypedCore::Clause(Clause::from(map)),
            "c_cons" => TypedCore::Cons(Cons::from(map)),
            "c_fun" => TypedCore::Fun(Fun::from(map)),
            "c_let" => TypedCore::Let(Let::from(map)),
            "c_letrec" => TypedCore::LetRec(LetRec::from(map)),
            "c_literal" => TypedCore::Literal(Literal::from(map)),
            "c_map" => TypedCore::Map(ErlMap::from(map)),
            "c_map_pair" => TypedCore::MapPair(MapPair::from(map)),
            "c_module" => TypedCore::Module(Module::from(map)),
            "c_opaque" => TypedCore::Opaque(Opaque::from(map)),
            "c_primop" => TypedCore::PrimOp(PrimOp::from(map)),
            "c_receive" => TypedCore::Receive(Receive::from(map)),
            "c_seq" => TypedCore::Seq(Seq::from(map)),
            "c_try" => TypedCore::Try(Try::from(map)),
            "c_tuple" => TypedCore::Tuple(Tuple::from(map)),
            "c_values" => TypedCore::Values(Values::from(map)),
            "c_var" => TypedCore::Var(Var::from(map)),
            type_name => panic!("{} not impled", type_name),
        }
    }
}

impl Display for TypedCore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TypedCore::AstList(x) => write!(f, "{}", x),
            TypedCore::AstTuple(x) => write!(f, "{}", x),
            TypedCore::Null(x) => write!(f, "{}", x),
            TypedCore::Bool(x) => write!(f, "{}", x),
            TypedCore::Number(x) => write!(f, "{}", x),
            TypedCore::String(x) => write!(f, "{}", x),
            TypedCore::Alias(x) => write!(f, "{}", x),
            TypedCore::Apply(x) => write!(f, "{}", x),
            TypedCore::Binary(x) => write!(f, "{}", x),
            TypedCore::BitStr(x) => write!(f, "{}", x),
            TypedCore::Call(x) => write!(f, "{}", x),
            TypedCore::Case(x) => write!(f, "{}", x),
            TypedCore::Catch(x) => write!(f, "{}", x),
            TypedCore::Clause(x) => write!(f, "{}", x),
            TypedCore::Cons(x) => write!(f, "{}", x),
            TypedCore::Fun(x) => write!(f, "{}", x),
            TypedCore::Let(x) => write!(f, "{}", x),
            TypedCore::LetRec(x) => write!(f, "{}", x),
            TypedCore::Literal(x) => write!(f, "{}", x),
            TypedCore::Map(x) => write!(f, "{}", x),
            TypedCore::MapPair(x) => write!(f, "{}", x),
            TypedCore::Module(x) => write!(f, "{}", x),
            TypedCore::Opaque(x) => write!(f, "{}", x),
            TypedCore::PrimOp(x) => write!(f, "{}", x),
            TypedCore::Receive(x) => write!(f, "{}", x),
            TypedCore::Seq(x) => write!(f, "{}", x),
            TypedCore::Try(x) => write!(f, "{}", x),
            TypedCore::Tuple(x) => write!(f, "{}", x),
            TypedCore::Values(x) => write!(f, "{}", x),
            TypedCore::Var(x) => write!(f, "{}", x),
        }
    }
}

impl Display for MaybeIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            MaybeIndex::Some(i) => write!(f, "{}: ", i),
            MaybeIndex::None => write!(f, ""),
        }
    }
}
