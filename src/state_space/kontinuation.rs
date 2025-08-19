use std::fmt::Display;

use super::{Env, KontinuationAddress, ProgLoc, ValueAddress};

// Kont :=
//       | List<Var>, ProgLoc, Env, KAddr // Let
//       | ProgLoc, Env, KAddr // Do i.e. sequencing
//       | Stop
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Kont<K: KontinuationAddress, V: ValueAddress> {
    Let(Vec<ProgLoc>, ProgLoc, Env<V>, K),
    Seq(ProgLoc, Env<V>, K),
    Stop,
}

impl<K: KontinuationAddress, V: ValueAddress> Display for Kont<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Kont::Let(vars, body, env, k_addr) => {
                write!(
                    f,
                    "Let({}, {}, {}, {})",
                    vars.iter()
                        .map(|var| { format!("{}", var) })
                        .collect::<Vec<String>>()
                        .join(", "),
                    body,
                    env,
                    k_addr
                )
            }
            Kont::Seq(next_prog_loc, env, k_addr) => {
                write!(f, "Seq({}, {}, {})", next_prog_loc, env, k_addr)
            }
            Kont::Stop => write!(f, "Stop"),
        }
    }
}
