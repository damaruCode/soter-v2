use super::{Env, KontinuationAddress, Pid, ProgLoc, Time, ValueAddress};
use std::fmt::Display;

// ProcState := (ProgLoc U+ Pid) x Env x KAddr x Time
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum ProgLocOrPid {
    ProgLoc(ProgLoc),
    Pid(Pid),
}

impl Display for ProgLocOrPid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgLocOrPid::Pid(pid) => write!(f, "Pid: {}", pid),
            ProgLocOrPid::ProgLoc(pl) => write!(f, "Pl: {}", pl),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct ProcState<K: KontinuationAddress, V: ValueAddress> {
    pub pid: Pid,
    pub prog_loc_or_pid: ProgLocOrPid,
    pub env: Env<V>,
    pub k_addr: K,
    pub time: Time,
}

impl<K: KontinuationAddress, V: ValueAddress> ProcState<K, V> {
    pub fn new(
        pid: Pid,
        prog_loc_or_pid: ProgLocOrPid,
        env: Env<V>,
        k_addr: K,
        time: Time,
    ) -> Self {
        ProcState {
            pid,
            prog_loc_or_pid,
            env,
            k_addr,
            time,
        }
    }

    pub fn init(init_k_addr: K) -> Self {
        ProcState {
            pid: Pid::init(),
            prog_loc_or_pid: ProgLocOrPid::ProgLoc(0),
            env: Env::init(),
            k_addr: init_k_addr,
            time: Time::init(),
        }
    }
}
