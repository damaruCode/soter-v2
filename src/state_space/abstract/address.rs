use std::hash::Hash;

use crate::{ast::VarName, util::SetMap};

use super::{Kont, ProcState, Value};

pub trait Address: Eq + Clone + Hash {}
impl<T: Eq + Clone + Hash> Address for T {}

pub trait KontinuationAddress: Address {}
pub trait ValueAddress: Address {}

pub trait AddressBuilder<'a, K: KontinuationAddress, V: ValueAddress> {
    /// Creates a new KontinuationAddress for the initial state of the abstract machine
    fn init_kaddr(proc_state: &'a ProcState<K, V>) -> K;

    /// Creates a new KontinuationAddress for any given state of the abstract machine
    fn new_kaddr(proc_state: &'a ProcState<K, V>, new_kont: &Kont<K, V>) -> K;

    /// Creates a new ValueAddress for any given state of the abstract machine
    fn new_vaddr(
        val_store: SetMap<V, Value<V>>,
        proc_state: &'a ProcState<K, V>,
        var: &VarName,
    ) -> V;
}
