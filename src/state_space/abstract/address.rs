use std::hash::Hash;

use super::{Kont, ProcState, ProgLoc, State, Var};

pub trait Address: Eq + Clone + Hash {}
impl<T: Eq + Clone + Hash> Address for T {}

pub trait KontinuationAddress: Address {}
pub trait ValueAddress: Address {}

pub trait AddressBuilder {
    type K: KontinuationAddress;
    type V: ValueAddress;

    /// Creates a new KontinuationAddress for the initial state of the abstract machine
    fn init_kaddr(proc_state: &ProcState<Self::K, Self::V>) -> Self::K;

    /// Creates a new KontinuationAddress for any given state of the abstract machine
    fn new_kaddr(
        proc_state: &ProcState<Self::K, Self::V>,
        new_kont: &Kont<Self::K, Self::V>,
    ) -> Self::K;

    /// Creates a new ValueAddress for any given state of the abstract machine
    fn new_vaddr(proc_state: &ProcState<Self::K, Self::V>, var: &Var) -> Self::V;
}
