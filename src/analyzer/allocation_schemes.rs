use crate::{
    ast::VarName,
    state_space::r#abstract::{
        AddressBuilder, Data, Env, Kont, KontinuationAddress, Pid, ProcState, ProgLoc,
        ProgLocOrPid, Time, Value, ValueAddress,
    },
    util::SetMap,
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
    fn init_kaddr(target_proc_state: &'a ProcState<KAddr<'a>, VAddr<'a>>) -> KAddr<'a> {
        KAddr {
            pid: target_proc_state.pid.clone(),
            prog_loc: match &target_proc_state.prog_loc_or_pid {
                ProgLocOrPid::ProgLoc(prog_loc) => prog_loc.clone(),
                _ => panic!("ProgLoc expected"),
            },
            env: target_proc_state.env.clone(),
            time: target_proc_state.time.clone(),
        }
    }

    fn new_kaddr(
        proc_state: &'a ProcState<KAddr<'a>, VAddr<'a>>,
        new_kont: &Kont<KAddr<'a>, VAddr<'a>>,
    ) -> KAddr<'a> {
        KAddr {
            pid: proc_state.pid.clone(),
            prog_loc: match &proc_state.prog_loc_or_pid {
                ProgLocOrPid::ProgLoc(prog_loc) => prog_loc.clone(),
                _ => panic!("ProgLoc expected"),
            },
            env: proc_state.env.clone(),
            time: proc_state.time.clone(),
        }
    }

    fn new_vaddr(
        val_store: SetMap<VAddr, Value<VAddr>>,
        proc_state: &'a ProcState<KAddr<'a>, VAddr<'a>>,
        var: &VarName,
    ) -> VAddr<'a> {
        VAddr {
            pid: proc_state.pid.clone(),
            var: var.clone(),
            data: match proc_state.prog_loc_or_pid {
                ProgLocOrPid::Pid(pid) => Data::Pid(pid),
                ProgLocOrPid::ProgLoc(prog_loc) => {
                    // TODO
                }
            },
            time: proc_state.time.clone(),
        }
    }
}
