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

impl Index for TypedCore {
    fn get_index(&self) -> Option<usize> {
        match self {
            TypedCore::AstTuple(ast) => ast.index,
            TypedCore::AstList(list) => list.index,

            TypedCore::Null(null) => null.index,
            TypedCore::Bool(bool) => bool.index,
            TypedCore::Number(number) => number.index,
            TypedCore::String(string) => string.index,

            TypedCore::Alias(alias) => alias.index,
            TypedCore::Apply(apply) => apply.index,
            TypedCore::Binary(binary) => binary.index,
            TypedCore::BitStr(bitstr) => bitstr.index,
            TypedCore::Call(call) => call.index,
            TypedCore::Case(case) => case.index,
            TypedCore::Catch(catch) => catch.index,
            TypedCore::Clause(clause) => clause.index,
            TypedCore::Cons(cons) => cons.index,
            TypedCore::Fun(fun) => fun.index,
            TypedCore::Let(let_) => let_.index,
            TypedCore::LetRec(letrec) => letrec.index,
            TypedCore::Literal(lit) => lit.index,
            TypedCore::Map(map) => map.index,
            TypedCore::MapPair(pair) => pair.index,
            TypedCore::Module(module) => module.index,
            TypedCore::Opaque(opq) => opq.index,
            TypedCore::PrimOp(prim) => prim.index,
            TypedCore::Receive(recv) => recv.index,
            TypedCore::Seq(seq) => seq.index,
            TypedCore::Try(try_) => try_.index,
            TypedCore::Tuple(tuple) => tuple.index,
            TypedCore::Values(vals) => vals.index,
            TypedCore::Var(var) => var.index,
        }
    }
}

impl From<Value> for TypedCore {
    fn from(core: Value) -> Self {
        match core {
            Value::Null => TypedCore::Null(ErlNull { index: None }),
            Value::Bool(bool) => TypedCore::Bool(ErlBool {
                inner: bool,
                index: None,
            }),
            Value::Number(number) => TypedCore::Number(ErlNumber {
                inner: number,
                index: None,
            }),
            Value::String(string) => TypedCore::String(ErlString {
                inner: string,
                index: None,
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
