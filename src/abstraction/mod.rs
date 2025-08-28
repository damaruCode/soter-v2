pub mod icfa;
pub mod p4f;
pub mod standard;
pub mod standard_v1cfa;

use serde::Serialize;

use crate::state_space::{
    Env, KontinuationAddress, ProcState, ProgLoc, ProgLocOrPid, Time, ValueAddress, VarName,
};

#[derive(clap::ValueEnum, Clone, Default, Debug, Serialize)]
pub enum AbstractionKind {
    /// Standard choice for KAddr and VAddr
    #[default]
    Standard,

    /// Standard choice with VAddr in 1-CFA
    StandardV1CFA,

    /// Adapted choice of KAddr according to P4F (Gilray et al.) in ICFA thesis
    P4F,

    /// Adapted choices of KAddr and VAddr according to the ICFA thesis including the P4F
    /// allocation strategy
    ICFA,
}

pub trait Abstraction<K: KontinuationAddress, V: ValueAddress> {
    /// Creates a new KontinuationAddress for the Stop continuation of the abstract machine
    fn stop_kaddr(&self) -> K;

    /// Creates a new KontinuationAddress for any given state of the abstract machine
    fn new_kaddr(
        &self,
        curr_proc_state: &ProcState<K, V>,
        next_prog_loc_or_pid: &ProgLocOrPid,
        next_env: &Env<V>,
        next_time: &Time,
    ) -> K;

    /// Creates a new ValueAddress for any given state of the abstract machine
    fn new_vaddr(
        &self,
        curr_proc_state: &ProcState<K, V>,
        var_name: &VarName,
        next_prog_loc_or_pid: &ProgLocOrPid,
        partial_next_env: &Env<V>,
        next_time: &Time,
    ) -> V;

    /// Creates a new Time given a previous Time
    fn tick(&self, curr_time: &Time, prog_loc: ProgLoc) -> Time;

    /// Creates a new Time by concatinating the Contours
    fn append_times(&self, pre_time: &Time, post_time: &Time) -> Time;
}
