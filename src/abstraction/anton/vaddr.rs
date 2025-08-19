use crate::state_space::{Pid, Time, ValueAddress, VarName};

// VAddr := Pid x Var x Data x Time
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct VAddr {
    pub pid: Pid,
    pub var_name: VarName,
    pub time: Time,
}
impl ValueAddress for VAddr {}
