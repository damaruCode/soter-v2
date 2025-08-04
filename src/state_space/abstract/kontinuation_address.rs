use super::{Env, Pid, ProgLoc, Time};

// KAddr := (Pid x ProgLoc x Env x Time) U+ {*}
// NOTE * might be possible to depict in control flow rather then as a  data struct
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct KAddr<'a> {
    pid: Pid<'a>,
    prog_loc: ProgLoc<'a>,
    env: Env<'a>,
    time: Time<'a>,
}

impl<'a> KAddr<'a> {
    pub fn init(prog_loc: ProgLoc<'a>) -> Self {
        KAddr {
            pid: Pid::init(prog_loc.clone()),
            prog_loc,
            env: Env::init(),
            time: Time::init(),
        }
    }

    pub fn new(pid: Pid<'a>, prog_loc: ProgLoc<'a>, env: Env<'a>, time: Time<'a>) -> Self {
        KAddr {
            pid,
            prog_loc,
            env,
            time,
        }
    }
}
