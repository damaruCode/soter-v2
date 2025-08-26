use crate::{
    ast::Literal,
    state_space::{Env, Pid, ProcState, ProgLoc, ProgLocOrPid, Time, VarName},
    util::plain_print,
};

use super::Abstraction;

pub mod kaddr;
pub mod vaddr;

pub use kaddr::KAddr;
pub use vaddr::VAddr;

pub struct P4FAbstraction {
    time_depth: usize,
}

impl P4FAbstraction {
    pub fn new(time_depth: usize) -> Self {
        Self { time_depth }
    }
}

impl Abstraction<KAddr, VAddr> for P4FAbstraction {
    fn stop_kaddr(&self) -> KAddr {
        KAddr {
            pid: Pid::init(),
            prog_loc: 0,
            env: Env::init(),
            time: Time::init(),
            _stop: true,
        }
    }

    fn new_kaddr(
        &self,
        curr_proc_state: &ProcState<KAddr, VAddr>,
        next_prog_loc_or_pid: &ProgLocOrPid,
        next_env: &Env<VAddr>,
        _next_time: &Time,
    ) -> KAddr {
        KAddr {
            pid: curr_proc_state.pid.clone(),
            prog_loc: match &next_prog_loc_or_pid {
                ProgLocOrPid::ProgLoc(prog_loc) => prog_loc.clone(),
                _ => panic!("ProgLoc expected"),
            },
            env: next_env.clone(),
            time: curr_proc_state.time.clone(),
            _stop: false,
        }
    }

    fn literal_vaddr(&self, literal: &Literal) -> VAddr {
        VAddr {
            pid: Pid::init(),
            var_name: VarName::Atom(plain_print::print(&*literal.val)),
            prog_loc_or_pid: ProgLocOrPid::ProgLoc(0),
            time: Time::init(),
        }
    }

    fn new_vaddr(
        &self,
        curr_proc_state: &ProcState<KAddr, VAddr>,
        var_name: &VarName,
        _next_prog_loc_or_pid: &ProgLocOrPid,
        _partial_next_env: &Env<VAddr>,
        _next_time: &Time,
    ) -> VAddr {
        VAddr {
            pid: curr_proc_state.pid.clone(),
            var_name: var_name.clone(),
            prog_loc_or_pid: curr_proc_state.prog_loc_or_pid.clone(),
            time: curr_proc_state.time.clone(),
        }
    }

    fn tick(&self, curr_time: &Time, prog_loc: ProgLoc) -> Time {
        let mut new_time = curr_time.clone();
        new_time.inner.push_back(prog_loc);

        if new_time.inner.len() > self.time_depth {
            new_time.inner.pop_front();
        }

        new_time
    }

    fn append_times(&self, pre_time: &Time, post_time: &Time) -> Time {
        let mut new_time = pre_time.clone();
        new_time.inner.append(&mut post_time.inner.clone());

        while new_time.inner.len() > self.time_depth {
            new_time.inner.pop_front();
        }

        new_time
    }
}
