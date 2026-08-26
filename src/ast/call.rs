use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_call, {anno=[] :: list(), module :: cerl:cerl(),
//		 name :: cerl:cerl(),
//		 args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Call {
    pub anno: AstList<TypedCore>,
    pub module: Box<TypedCore>,
    pub name: Box<TypedCore>,
    pub args: AstList<TypedCore>,
    pub index: MaybeIndex,
}

impl Call {
    pub fn new() -> Self {
        Self {
            anno: AstList::from(TypedCore::new()),
            module: Box::new(TypedCore::new()),
            name: Box::new(TypedCore::new()),
            args: AstList::from(TypedCore::new()),
            index: MaybeIndex::None,
        }
    }
}

impl From<Map<String, Value>> for Call {
    fn from(map: Map<String, Value>) -> Self {
        Call {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            module: Box::new(TypedCore::from(map.get("module").unwrap().clone())),
            name: Box::new(TypedCore::from(map.get("name").unwrap().clone())),
            args: AstList::from(map.get("args").unwrap().as_array().unwrap().clone()),
            index: MaybeIndex::None,
        }
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}call {}:{} ({})",
            self.index, self.module, self.name, self.args
        )
    }
}
