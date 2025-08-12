use super::{Env, ProgLoc, ValueAddress};

// Closure := ProgLoc x Env
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Closure<'a, V: ValueAddress> {
    pub prog_loc: ProgLoc<'a>,
    pub env: Env<V>,
}
