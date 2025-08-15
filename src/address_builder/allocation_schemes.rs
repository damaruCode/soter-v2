use crate::state_space::r#abstract::{
    AddressBuilder, Env, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Time, ValueAddress,
    VarName,
};

// KAddr := (Pid x ProgLoc x Env x Time) U+ {*}
// NOTE * might be possible to depict in control flow rather then as a  data struct
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct KAddr {
    pid: Pid,
    prog_loc: usize,
    env: Env<VAddr>,
    time: Time,
}

impl KontinuationAddress for KAddr {}

// VAddr := Pid x Var x Data x Time
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct VAddr {
    pid: Pid,
    var_name: VarName,
    time: Time,
}
impl ValueAddress for VAddr {}

pub struct StandardAddressBuilder;

impl StandardAddressBuilder {
    pub fn new() -> Self {
        StandardAddressBuilder {}
    }
}

impl AddressBuilder<KAddr, VAddr> for StandardAddressBuilder {
    fn init_kaddr(&self) -> KAddr {
        KAddr {
            pid: Pid::init(),
            prog_loc: 0,
            env: Env::init(),
            time: Time::init(),
        }
    }

    fn new_kaddr(
        &self,
        curr_proc_state: &ProcState<KAddr, VAddr>,
        _next_prog_loc_or_pid: &ProgLocOrPid,
        _next_env: &Env<VAddr>,
        _next_time: &Time,
    ) -> KAddr {
        KAddr {
            pid: curr_proc_state.pid.clone(),
            prog_loc: match &curr_proc_state.prog_loc_or_pid {
                ProgLocOrPid::ProgLoc(prog_loc) => prog_loc.clone(),
                _ => panic!("ProgLoc expected"),
            },
            env: curr_proc_state.env.clone(),
            time: curr_proc_state.time.clone(),
        }
    }

    fn new_vaddr(
        &self,
        curr_proc_state: &ProcState<KAddr, VAddr>,
        var_name: &VarName,
        _next_prog_loc_or_pid: &ProgLocOrPid,
        _next_time: &Time,
    ) -> VAddr {
        VAddr {
            pid: curr_proc_state.pid.clone(),
            var_name: var_name.clone(),
            time: curr_proc_state.time.clone(),
        }
    }
}
