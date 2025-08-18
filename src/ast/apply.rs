use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_apply, {anno=[] :: list(), op :: cerl:cerl(),
//		  args :: [cerl:cerl()]}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Apply {
    pub anno: AstList<TypedCore>,
    pub op: Box<TypedCore>,
    pub args: AstList<TypedCore>,
    pub index: MaybeIndex,
}

impl From<Map<String, Value>> for Apply {
    fn from(map: Map<String, Value>) -> Self {
        Apply {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            op: Box::new(TypedCore::from(map.get("op").unwrap().clone())),
            args: AstList::from(map.get("args").unwrap().as_array().unwrap().clone()),
            index: MaybeIndex::None,
        }
    }
}

impl Display for Apply {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}apply {}({})", self.index, *self.op, self.args)
    }
}
