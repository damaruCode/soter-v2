use crate::ast::*;
use serde::{Deserialize, Serialize};

///-record(c_let, {anno=[] :: list(), vars :: [cerl:cerl()],
///		arg :: cerl:cerl(),
///		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Let {
    pub anno: AstList<TypedCore>,
    pub vars: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub body: Box<TypedCore>,
    pub index: MaybeIndex,
}

impl Let {
    pub fn new() -> Self {
        Self {
            anno: AstList::from(TypedCore::new()),
            vars: AstList::from(TypedCore::new()),
            arg: Box::new(TypedCore::new()),
            body: Box::new(TypedCore::new()),
            index: MaybeIndex::None,
        }
    }
}

impl From<Map<String, Value>> for Let {
    fn from(map: Map<String, Value>) -> Self {
        Let {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            vars: AstList::from(map.get("vars").unwrap().as_array().unwrap().clone()),
            arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
            body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
            index: MaybeIndex::None,
        }
    }
}

impl Display for Let {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}let <{}> = {} in {}",
            self.index, self.vars, *self.arg, *self.body
        )
    }
}
