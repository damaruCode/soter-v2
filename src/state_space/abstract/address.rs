use std::hash::Hash;

use super::{Kont, ProcState, Var};

pub trait Address: Eq + Clone + Hash {}
impl<T: Eq + Clone + Hash> Address for T {}

pub trait KontinuationAddress: Address {
    fn init(proc_state: &ProcState) -> Self;
    fn new(proc_state: &ProcState, new_kont: &Kont) -> Self;
}
pub trait ValueAddress: Address {
    fn new(process_state: &ProcState, var: &Var) -> Self;
}
