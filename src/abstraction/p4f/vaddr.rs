use std::fmt::Display;

use crate::state_space::{Pid, ProgLocOrPid, Time, ValueAddress, VarName};

// VAddr := Pid x Var x Data x Time
#[derive(Eq, PartialEq, Clone, Debug, PartialOrd, Ord)]
pub struct VAddr {
    pub pid: Pid,
    pub var_name: VarName,
    pub call_site_prog_loc: ProgLocOrPid,
    pub time: Time,
}
impl ValueAddress for VAddr {}

impl Display for VAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}, {}, {}, {})",
            self.pid, self.var_name, self.call_site_prog_loc, self.time
        )
    }
}
