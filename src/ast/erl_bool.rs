use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct ErlBool {
    pub inner: bool,
    pub index: Option<usize>,
}
