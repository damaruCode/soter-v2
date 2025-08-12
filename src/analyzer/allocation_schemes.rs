use crate::{
    ast::{TypedCore, VarName},
    state_space::r#abstract::{
        AddressBuilder, Data, Env, KontinuationAddress, Pid, ProcState, ProgLoc, ProgLocOrPid,
        Time, ValueAddress,
    },
};

// KAddr := (Pid x ProgLoc x Env x Time) U+ {*}
// NOTE * might be possible to depict in control flow rather then as a  data struct
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct KAddr<'a> {
    pid: Pid<'a>,
    prog_loc: ProgLoc<'a>,
    env: Env<VAddr<'a>>,
    time: Time<'a>,
}
impl KontinuationAddress for KAddr<'_> {}

// VAddr := Pid x Var x Data x Time
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct VAddr<'a> {
    pid: Pid<'a>,
    var: VarName,
    data: Data<'a>,
    time: Time<'a>,
}
impl ValueAddress for VAddr<'_> {}

pub struct StandardAddressBuilder {}
impl<'a> AddressBuilder<'a, KAddr<'a>, VAddr<'a>> for StandardAddressBuilder {
    fn init_kaddr(
        &self,
        pid: Pid<'a>,
        prog_loc: ProgLoc<'a>,
        env: Env<VAddr<'a>>,
        time: Time<'a>,
    ) -> KAddr<'a> {
        KAddr {
            pid,
            prog_loc,
            env,
            time,
        }
    }

    fn new_kaddr(
        &self,
        curr_proc_state: &'a ProcState<KAddr<'a>, VAddr<'a>>,
        _next_prog_loc_or_pid: &'a ProgLocOrPid,
        _next_env: &'a Env<VAddr<'a>>,
        _next_time: &'a Time,
    ) -> KAddr<'a> {
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
        curr_proc_state: &'a ProcState<KAddr<'a>, VAddr<'a>>,
        var: &VarName,
        _next_prog_loc_or_pid: &'a ProgLocOrPid,
        _next_env: &'a Env<VAddr<'a>>,
        _next_time: &'a Time,
    ) -> VAddr<'a> {
        VAddr {
            pid: curr_proc_state.pid.clone(),
            var: var.clone(),
            data: match &curr_proc_state.prog_loc_or_pid {
                ProgLocOrPid::Pid(pid) => Data::Pid(pid.clone()),
                ProgLocOrPid::ProgLoc(prog_loc) => match prog_loc.get() {
                    TypedCore::Fun(fun) => Data::Fun(fun),
                    // TODO how would we recognize a Constructor or Atom?
                    _ => panic!("Unexpected program location"),
                },
            },
            time: curr_proc_state.time.clone(),
        }
    }
}
