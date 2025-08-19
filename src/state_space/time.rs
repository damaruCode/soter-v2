use std::{collections::VecDeque, fmt::Display};

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

impl Display for Time {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.inner
                .iter()
                .map(|prog_loc| { format!("{}", prog_loc) })
                .collect::<Vec<String>>()
                .join(" "),
        )
    }
}
