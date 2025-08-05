use crate::ast::VarName;

use super::{Data, Pid, Time};

// VAddr := Pid x Var x Data x Time
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct VAddr<'a> {
    pid: Pid<'a>,
    var_name: VarName,
    data: Data<'a>,
    time: Time<'a>,
}

impl<'a> VAddr<'a> {
    fn new(pid: Pid<'a>, var_name: VarName, data: Data<'a>, time: Time<'a>) -> Self {
        VAddr {
            pid,
            var_name,
            data,
            time,
        }
    }
}
