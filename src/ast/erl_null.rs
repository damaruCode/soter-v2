use std::fmt::Display;

use serde::{Deserialize, Serialize};

use super::MaybeIndex;

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Hash, Clone)]
pub struct ErlNull {
    pub index: MaybeIndex,
}

impl Display for ErlNull {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
