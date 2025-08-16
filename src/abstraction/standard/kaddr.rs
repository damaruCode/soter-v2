use crate::state_space::{Env, KontinuationAddress, Pid, Time};

use super::VAddr;

// KAddr := (Pid x ProgLoc x Env x Time) U+ {*}
// NOTE * might be possible to depict in control flow rather then as a  data struct
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct KAddr {
    pub pid: Pid,
    pub prog_loc: usize,
    pub env: Env<VAddr>,
    pub time: Time,
    pub _stop: bool,
}

impl KontinuationAddress for KAddr {}
