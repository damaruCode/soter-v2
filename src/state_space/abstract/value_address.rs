use super::{Data, Pid, Time, Var};

// VAddr := Pid x Var x Data x Time
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct VAddr<'a> {
    pid: Pid<'a>,
    var: Var<'a>,
    data: Data<'a>,
    time: Time<'a>,
}

impl<'a> VAddr<'a> {
    fn new(pid: Pid<'a>, var: Var<'a>, data: Data<'a>, time: Time<'a>) -> Self {
        VAddr {
            pid,
            var,
            data,
            time,
        }
    }
}
