use super::{Env, ProgLoc, ValueAddress};

// Closure := ProgLoc x Env
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Closure<V: ValueAddress> {
    pub prog_loc: ProgLoc,
    pub env: Env<V>,
}
