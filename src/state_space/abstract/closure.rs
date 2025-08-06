use super::{Env, ProgLoc, ValueAddress};

// Closure := ProgLoc x Env
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Closure<'a, V: ValueAddress> {
    prog_loc: ProgLoc<'a>,
    env: Env<V>,
}
