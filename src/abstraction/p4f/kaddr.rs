use std::fmt::Display;

use crate::state_space::{Env, KontinuationAddress, Pid, Time};

use super::VAddr;

// KAddr := (Pid x ProgLoc x Env x Time) U+ {*}
// NOTE * might be possible to depict in control flow rather then as a  data struct
#[derive(Eq, PartialEq, Clone, Debug, PartialOrd, Ord)]
pub struct KAddr {
    pub pid: Pid,
    pub prog_loc: usize,
    pub return_site_env: Env<VAddr>,
    pub time: Time,
    pub _stop: bool,
}

impl KontinuationAddress for KAddr {}

impl Display for KAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}, {}, {},  {}, {})",
            self.pid, self.prog_loc, self.return_site_env, self.time, self._stop
        )
    }
}
