use super::{Env, KontinuationAddress, ValueAddress};
use crate::ast::Var;

// Kont :=
//       | List<Name>, ProgLoc, Env, KAddr // Let
//       | ProgLoc, Env, KAddr // Do i.e. sequencing
//       | Stop
// NOTE Stop might be possible to depict in control flow rather then as a data struct
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Kont<K: KontinuationAddress, V: ValueAddress> {
    Let(Vec<Var>, usize, Env<V>, K),
    Do(usize, Env<V>, K),
    Stop,
}
