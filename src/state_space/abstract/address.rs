use super::{Env, ProcState, ProgLocOrPid, Time, VarName};

use std::fmt::Debug;

pub trait Address: Eq + Clone + Debug {}

impl<T: Eq + Clone + Debug> Address for T {}

pub trait KontinuationAddress: Address {}

pub trait ValueAddress: Address {}

pub trait AddressBuilder<K: KontinuationAddress, V: ValueAddress> {
    /// Creates a new KontinuationAddress for the stop continuation of the abstract machine
    fn init_kaddr(&self) -> K;

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
        next_time: &Time,
    ) -> V;
}
