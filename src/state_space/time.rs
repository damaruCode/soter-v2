use std::collections::VecDeque;

use super::ProgLoc;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Time {
    pub inner: VecDeque<ProgLoc>,
}

impl Time {
    pub fn init() -> Self {
        Self {
            inner: VecDeque::new(),
        }
    }
    pub fn append(&mut self, mut vec: VecDeque<ProgLoc>) {
        self.inner.append(&mut vec);
    }
}
