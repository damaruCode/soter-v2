use crate::ast::*;
use serde_json::{Deserialize, Serialize};

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
