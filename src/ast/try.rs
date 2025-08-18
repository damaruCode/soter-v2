use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_try, {anno=[] :: list(), arg :: cerl:cerl(),
//		vars :: [cerl:cerl()],
//		body :: cerl:cerl(),
//		evars :: [cerl:cerl()],
//		handler :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Try {
    pub anno: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub vars: AstList<TypedCore>,
    pub body: Box<TypedCore>,
    pub evars: AstList<TypedCore>,
    pub handler: Box<TypedCore>,
    pub index: MaybeIndex,
}

impl From<Map<String, Value>> for Try {
    fn from(map: Map<String, Value>) -> Self {
        Try {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().clone()),
            arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
            vars: AstList::from(map.get("vars").unwrap().as_array().unwrap().clone()),
            body: Box::new(TypedCore::from(map.get("body").unwrap().clone())),
            evars: AstList::from(map.get("evars").unwrap().as_array().unwrap().clone()),
            handler: Box::new(TypedCore::from(map.get("handler").unwrap().clone())),
            index: MaybeIndex::None,
        }
    }
}

impl Display for Try {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
