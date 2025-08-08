use crate::ast::Var;

use super::{Data, Pid, Time};

// VAddr := Pid x Var x Data x Time
#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct VAddr<'a> {
    pid: Pid<'a>,
    var: Var,
    data: Data<'a>,
    time: Time<'a>,
}

impl<'a> VAddr<'a> {
    fn new(pid: Pid<'a>, var: Var, data: Data<'a>, time: Time<'a>) -> Self {
        VAddr {
            pid,
            var,
            data,
            time,
        }
    }
}
