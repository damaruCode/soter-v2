use crate::{
    ast::VarName,
    state_space::r#abstract::{
        AddressBuilder, Data, Env, KontinuationAddress, Pid, ProcState, ProgLoc, ProgLocOrPid,
        State, Time, ValueAddress,
    },
};

// KAddr := (Pid x ProgLoc x Env x Time) U+ {*}
// NOTE * might be possible to depict in control flow rather then as a  data struct
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct KAddr<'a> {
    pid: Pid<'a>,
    prog_loc: ProgLoc<'a>,
    env: Env<'a, VAddr<'a>>,
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
impl<'a> AddressBuilder<KAddr<'a>, VAddr<'a>> for StandardAddressBuilder {
    fn init_kaddr(target_proc_state: &ProcState<KAddr, VAddr>) -> KAddr {
        KAddr {
            pid: target_proc_state.pid,
            prog_loc: match target_proc_state.prog_loc_or_pid {
                ProgLocOrPid::ProgLoc(prog_loc) => prog_loc,
                ProgLocOrPid::Pid(_pid) => panic!("ProgLoc expected"),
            },
            env: target_proc_state.env,
            time: target_proc_state.time,
        }
    }

    fn new_kaddr(state: &State<KAddr, VAddr>, partial_info: &State<KAddr, VAddr>) -> Self::K {}

    fn new_vaddr(state: &State<KAddr, VAddr>, partial_info: &State<KAddr, VAddr>) -> Self::V {}
}
