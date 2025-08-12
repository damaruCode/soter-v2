use crate::{
    ast::{TypedCore, VarName},
    state_space::r#abstract::{
        AddressBuilder, Data, Env, KontinuationAddress, Pid, ProcState, ProgLocOrPid, Time,
        ValueAddress,
    },
};

// KAddr := (Pid x ProgLoc x Env x Time) U+ {*}
// NOTE * might be possible to depict in control flow rather then as a  data struct
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct KAddr {
    pid: Pid,
    prog_loc: usize,
    env: Env<VAddr>,
    time: Time,
}

impl KontinuationAddress for KAddr {}

// VAddr := Pid x Var x Data x Time
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct VAddr {
    pid: Pid,
    var: VarName,
    data: Data,
    time: Time,
}
impl ValueAddress for VAddr {}

pub struct StandardAddressBuilder {}
impl AddressBuilder<KAddr, VAddr> for StandardAddressBuilder {
    fn init_kaddr(&self) -> KAddr {
        KAddr {
            pid: Pid::init(),
            prog_loc: 0,
            env: Env::init(),
            time: Time::init(),
        }
    }

    //TODO
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

    //TODO
    fn new_vaddr(
        &self,
        curr_proc_state: &ProcState<KAddr, VAddr>,
        var: &VarName,
        _next_prog_loc_or_pid: &ProgLocOrPid,
        _next_env: &Env<VAddr>,
        _next_time: &Time,
    ) -> VAddr {
        VAddr {
            pid: curr_proc_state.pid.clone(),
            var: var.clone(),
            data: match &curr_proc_state.prog_loc_or_pid {
                ProgLocOrPid::Pid(pid) => Data::Pid(pid.clone()),
                //TODO with ast_helper
                //ProgLocOrPid::ProgLoc(prog_loc) => match self.ast_helper.get(prog_loc).unwrap() {
                //    TypedCore::Fun(fun) => Data::Fun(fun),
                //    // TODO how would we recognize a Constructor or Atom?
                //    _ => panic!("Unexpected program location"),
                //},
                _ => Data::Pid(Pid::init()),
            },
            time: curr_proc_state.time.clone(),
        }
    }
}
