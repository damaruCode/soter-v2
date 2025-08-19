use std::fmt::{Debug, Display};

pub trait Address: Eq + Clone + Debug + Display {}

impl<T: Eq + Clone + Debug + Display> Address for T {}

pub trait KontinuationAddress: Address {}

pub trait ValueAddress: Address {}
