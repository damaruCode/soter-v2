use std::fmt::Display;

use crate::state_space::{Pid, Time, ValueAddress, VarName};

// VAddr := Pid x Var x Data x Time
#[derive(Eq, PartialEq, Clone, Debug, Ord, PartialOrd)]
pub struct VAddr {
    pub pid: Pid,
    pub var_name: VarName,
    pub time: Time,
}
impl ValueAddress for VAddr {}

impl Display for VAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {}, {})", self.pid, self.var_name, self.time)
    }
}
