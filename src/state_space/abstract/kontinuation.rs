use super::{Env, KAddr, ProgLoc};
use crate::ast::{AstList, Var};

// Kont :=
//       | List<Name>, ProgLoc, Env, KAddr // Let
//       | ProgLoc, Env, KAddr // Do
//       | Stop
// NOTE Stop might be possible to depict in control flow rather then as a data struct
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Kont<'a> {
    Let(&'a AstList<Var>, ProgLoc<'a>, Env<'a>, KAddr<'a>),
    Do(ProgLoc<'a>, Env<'a>, KAddr<'a>),
    Stop,
}
