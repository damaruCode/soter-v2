use crate::ast::*;
use serde::{Deserialize, Serialize};

//-record(c_map, {anno=[] :: list(),
//		     arg=#c_literal{val=#{}} | cerl:c_var() | cerl:c_literal(), // NOTE maybe this ???
//		     arg=#c_literal{val=#{}} :: cerl:c_var() | cerl:c_literal(),
//		     es :: [cerl:c_map_pair()],
//		     is_pat=false :: boolean()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct LiteralMap {
    pub anno: AstList<TypedCore>,
    pub arg: Literal,
    pub es: AstList<MapPair>,
    pub is_pat: bool,
}
impl From<Map<String, Value>> for LiteralMap {
    fn from(map: Map<String, Value>) -> Self {
        LiteralMap {
            anno: AstList::from(map.get("anno").unwrap().as_array().unwrap().to_vec()),
            arg: Literal::from(map.get("arg").unwrap().clone()),
            es: AstList::from(map.get("es").unwrap().as_array().unwrap().to_vec()),
            is_pat: map.get("is_pat").unwrap().as_bool().unwrap(),
        }
    }
}
