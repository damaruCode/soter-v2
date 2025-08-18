use std::fmt::Display;

use serde::{Deserialize, Serialize};
use serde_json::Number;

use super::MaybeIndex;

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct ErlNumber {
    pub inner: Number,
    pub index: MaybeIndex,
}

impl Display for ErlNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.index, self.inner)
    }
}
