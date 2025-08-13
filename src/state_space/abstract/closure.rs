use super::{Env, ValueAddress};

// Closure := ProgLoc x Env
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Closure<V: ValueAddress> {
    pub prog_loc: usize,
    pub env: Env<V>,
}
