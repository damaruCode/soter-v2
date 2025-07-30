//-record(c_fun, {anno=[] :: list(), vars :: [cerl:cerl()],
//		body :: cerl:cerl()}).
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct Fun {
    pub anno: AstList<TypedCore>,
    pub vars: AstList<TypedCore>,
    pub body: Box<TypedCore>,
}
