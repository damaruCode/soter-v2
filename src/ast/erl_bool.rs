use std::fmt::Display;

use serde::{Deserialize, Serialize};

use super::MaybeIndex;

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct ErlBool {
    pub inner: bool,
    pub index: MaybeIndex,
}

impl Display for ErlBool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.index, self.inner)
    }
}
