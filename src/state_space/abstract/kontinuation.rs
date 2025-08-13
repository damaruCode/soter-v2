use super::{Env, KontinuationAddress, ValueAddress};

type Var = usize;
type ProgLoc = usize;

// Kont :=
//       | List<Name>, ProgLoc, Env, KAddr // Let
//       | ProgLoc, Env, KAddr // Do i.e. sequencing
//       | Stop
// NOTE Stop might be possible to depict in control flow rather then as a data struct
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Kont<K: KontinuationAddress, V: ValueAddress> {
    Let(Vec<Var>, ProgLoc, Env<V>, K),
    Do(ProgLoc, Env<V>, K),
    Stop,
}
