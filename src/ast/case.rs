use std::fmt::Display;

use crate::state_space::{Value, ValueAddress, VarName};
use serde::{Deserialize, Serialize};
use serde_json::Map;

use super::{AstList, MaybeIndex, TypedCore};

pub enum VarNamesOrValue<V: ValueAddress> {
    VarNames(Vec<VarName>),
    Value(Value<V>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ValueAddressOrValue<V: ValueAddress> {
    ValueAddress(V),
    Value(Value<V>),
}

/// -record(c_case, {anno=[] :: list(), arg :: cerl:cerl(),
///    clauses :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Case {
    pub anno: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub clauses: AstList<TypedCore>,
    pub index: MaybeIndex,
}

impl Default for Case {
    fn default() -> Self {
        Self::new()
    }
}

impl Case {
    pub fn new() -> Self {
        Self {
            anno: AstList::from(TypedCore::new()),
            arg: Box::new(TypedCore::new()),
            clauses: AstList::from(TypedCore::new()),
            index: MaybeIndex::None,
        }
    }
}

impl From<Map<String, serde_json::Value>> for Case {
    fn from(map: Map<String, serde_json::Value>) -> Self {
        Case {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
            clauses: AstList::from(map.get("clauses").unwrap().as_array().unwrap().clone()),
            index: MaybeIndex::None,
        }
    }
}

impl Display for Case {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}case <{}> of {}", self.index, self.arg, self.clauses)
    }
}
