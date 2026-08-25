use std::fmt::Display;

use serde::{Deserialize, Serialize};

use super::MaybeIndex;

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct ErlString {
    pub inner: String,
    pub index: MaybeIndex,
}

impl ErlString {
    pub fn new(string: String) -> Self {
        Self {
            inner: string,
            index: MaybeIndex::None,
        }
    }
}

impl Display for ErlString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.index, self.inner)
    }
}
