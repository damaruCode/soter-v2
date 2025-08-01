use super::{Env, ProgLoc};

// Closure := ProgLoc x Env
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Closure<'a> {
    prog_loc: ProgLoc<'a>,
    env: Env<'a>,
}
