use super::{Env, ProcState, ProgLocOrPid, Time};
use crate::ast::Var;

pub trait Address: Eq + Clone {}

impl<T: Eq + Clone> Address for T {}

pub trait KontinuationAddress: Address {}

pub trait ValueAddress: Address {}

pub trait AddressBuilder<K: KontinuationAddress, V: ValueAddress> {
    /// Creates a new KontinuationAddress for the initial state of the abstract machine
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
        var: &Var,
        next_prog_loc_or_pid: &ProgLocOrPid,
        next_env: &Env<V>,
        next_time: &Time,
    ) -> V;
}
