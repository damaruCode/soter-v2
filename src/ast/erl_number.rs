use serde::{Deserialize, Serialize};
use serde_json::Number;

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct ErlNumber {
    pub inner: Number,
    pub index: Option<usize>,
}
