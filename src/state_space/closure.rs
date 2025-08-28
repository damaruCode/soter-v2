use std::fmt::Display;

use super::{Env, ProgLoc, ValueAddress};

// Closure := ProgLoc x Env
#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Closure<V: ValueAddress> {
    pub prog_loc: ProgLoc,
    pub env: Env<V>,
}

impl<V: ValueAddress> Display for Closure<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.prog_loc, self.env)
    }
}
