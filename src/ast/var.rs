use crate::ast::*;
use serde::{Deserialize, Serialize};

/// -record(c_var, {anno=[] :: list(), name :: cerl:var_name()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Var {
    pub anno: AstList<TypedCore>,
    pub name: Box<TypedCore>,
    pub var_id: MaybeIndex,
    pub index: MaybeIndex,
}

impl Default for Var {
    fn default() -> Self {
        Self::new()
    }
}

impl Var {
    pub fn new() -> Self {
        Self {
            anno: AstList::from(TypedCore::new()),
            name: Box::new(TypedCore::new()),
            var_id: MaybeIndex::None,
            index: MaybeIndex::None,
        }
    }
}

impl From<Map<String, Value>> for Var {
    fn from(map: Map<String, Value>) -> Self {
        Var {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            name: Box::new(TypedCore::from(map.get("name").unwrap().clone())),
            var_id: MaybeIndex::None,
            index: MaybeIndex::None,
        }
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}var {}{}",
            self.index,
            *self.name,
            match self.var_id {
                MaybeIndex::Some(var_id) => format!(".{var_id}"),
                MaybeIndex::None => "".to_string(),
            }
        )
    }
}
