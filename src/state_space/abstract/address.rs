use std::hash::Hash;

use crate::{ast::VarName, util::SetMap};

use super::{Env, Kont, Pid, ProcState, ProgLoc, ProgLocOrPid, Time, Value};

pub trait Address: Eq + Clone + Hash {}
impl<T: Eq + Clone + Hash> Address for T {}

pub trait KontinuationAddress: Address {}
pub trait ValueAddress: Address {}

pub trait AddressBuilder<'a, K: KontinuationAddress, V: ValueAddress> {
    /// Creates a new KontinuationAddress for the initial state of the abstract machine
    fn init_kaddr(&self, pid: Pid<'a>, prog_loc: ProgLoc<'a>, env: Env<V>, time: Time<'a>) -> K;

    /// Creates a new KontinuationAddress for any given state of the abstract machine
    fn new_kaddr(
        &self,
        curr_proc_state: &'a ProcState<K, V>,
        next_prog_loc_or_pid: &'a ProgLocOrPid,
        next_env: &'a Env<V>,
        next_time: &'a Time,
    ) -> K;

    /// Creates a new ValueAddress for any given state of the abstract machine
    fn new_vaddr(
        &self,
        curr_proc_state: &'a ProcState<K, V>,
        var: &VarName,
        next_prog_loc_or_pid: &'a ProgLocOrPid,
        next_env: &'a Env<V>,
        next_time: &'a Time,
    ) -> V;
}
