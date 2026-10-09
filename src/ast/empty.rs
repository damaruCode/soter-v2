use serde::{Deserialize, Serialize};

use crate::ast::MaybeIndex;

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct Empty {
    pub index: MaybeIndex,
}

impl Default for Empty {
    fn default() -> Self {
        Self::new()
    }
}

impl Empty {
    pub fn new() -> Self {
        Self {
            index: MaybeIndex::None,
        }
    }
}
