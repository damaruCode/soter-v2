use std::fmt::{Debug, Display};

pub trait Address: Eq + Clone + Debug + Display + Ord + PartialOrd {}

impl<T: Eq + Clone + Debug + Display + Ord + PartialOrd> Address for T {}

pub trait KontinuationAddress: Address {}

pub trait ValueAddress: Address {}
