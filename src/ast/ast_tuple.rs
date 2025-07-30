use serde::{Deserialize, Serialize};
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct AstTuple<T> {
    pub frst: Box<T>,
    pub scnd: Box<T>,
}
