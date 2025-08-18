use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_map, {anno=[] :: list(),
//		     arg=#c_literal{val=#{}} | cerl:c_var() | cerl:c_literal(), // NOTE maybe this ???
//		     arg=#c_literal{val=#{}} :: cerl:c_var() | cerl:c_literal(),
//		     es :: [cerl:c_map_pair()],
//		     is_pat=false :: boolean()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct ErlMap {
    pub anno: AstList<TypedCore>,
    pub arg: Box<TypedCore>,
    pub es: AstList<TypedCore>,
    pub is_pat: bool,
    pub index: MaybeIndex,
}

impl From<Map<String, Value>> for ErlMap {
    fn from(map: Map<String, Value>) -> Self {
        ErlMap {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().to_vec()),
            arg: Box::new(TypedCore::from(map.get("arg").unwrap().clone())),
            es: AstList::from(map.get("es").unwrap().as_array().unwrap().to_vec()),
            is_pat: map.get("is_pat").unwrap().as_bool().unwrap(),
            index: MaybeIndex::None,
        }
    }
}

impl Display for ErlMap {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
