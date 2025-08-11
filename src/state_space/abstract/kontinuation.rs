use super::{Env, KontinuationAddress, ProgLoc, ValueAddress};
use crate::ast::{AstList, Var};

// Kont :=
//       | List<Name>, ProgLoc, Env, KAddr // Let
//       | ProgLoc, Env, KAddr // Do i.e. sequencing
//       | Stop
// NOTE Stop might be possible to depict in control flow rather then as a data struct
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Kont<'a, K: KontinuationAddress, V: ValueAddress> {
    Let(&'a AstList<Var>, ProgLoc<'a>, Env<V>, K),
    Do(ProgLoc<'a>, Env<V>, K),
    Stop,
}
