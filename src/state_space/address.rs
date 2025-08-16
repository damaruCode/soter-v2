use std::fmt::Debug;

pub trait Address: Eq + Clone + Debug {}

impl<T: Eq + Clone + Debug> Address for T {}

pub trait KontinuationAddress: Address {}

pub trait ValueAddress: Address {}
