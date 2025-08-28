use std::{collections::VecDeque, fmt::Display};

use super::{Env, KontinuationAddress, ProgLoc, Value, ValueAddress};

// Kont :=
//       | List<Var>, ProgLoc, Env, KAddr // Let
//       | ProgLoc, Env, KAddr // Do i.e. sequencing
//       | Stop
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Kont<K: KontinuationAddress, V: ValueAddress> {
    Let(Vec<ProgLoc>, ProgLoc, Env<V>, K),
    Apply(VecDeque<ProgLoc>, Vec<Value<V>>, Value<V>, Env<V>, K),
    Spawn(K),
    Send(ProgLoc, K),
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
            Kont::Apply(arg_list, value_list, op_value, env, k_addr) => {
                write!(
                    f,
                    "Apply({}, {}, {}, {}, {})",
                    arg_list
                        .iter()
                        .map(|var| { format!("{}", var) })
                        .collect::<Vec<String>>()
                        .join(", "),
                    value_list
                        .iter()
                        .map(|var| { format!("{}", var) })
                        .collect::<Vec<String>>()
                        .join(", "),
                    op_value,
                    env,
                    k_addr
                )
            }
            Kont::Send(msg_prog_loc, k_addr) => {
                write!(f, "Send({}, {})", msg_prog_loc, k_addr)
            }
            Kont::Spawn(k_addr) => {
                write!(f, "Spawn({})", k_addr)
            }
            Kont::Seq(next_prog_loc, env, k_addr) => {
                write!(f, "Seq({}, {}, {})", next_prog_loc, env, k_addr)
            }
            Kont::Stop => write!(f, "Stop"),
        }
    }
}
