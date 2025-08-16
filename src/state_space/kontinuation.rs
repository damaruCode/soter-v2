use super::{Env, KontinuationAddress, ProgLoc, ValueAddress};

// Kont :=
//       | List<Var>, ProgLoc, Env, KAddr // Let
//       | ProgLoc, Env, KAddr // Do i.e. sequencing
//       | Stop
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Kont<K: KontinuationAddress, V: ValueAddress> {
    Module(usize, Env<V>, K),
    Let(Vec<ProgLoc>, ProgLoc, Env<V>, K),
    Stop,
}
