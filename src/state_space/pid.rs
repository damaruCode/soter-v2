use std::fmt::Display;

use super::{ProgLoc, Time};

// Pid := ProgLoc x Time
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Pid {
    pub prog_loc: ProgLoc,
    pub time: Time,
}

impl Pid {
    pub fn init() -> Self {
        Pid {
            prog_loc: 0,
            time: Time::init(),
        }
    }
}

impl Display for Pid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.prog_loc, self.time)
    }
}
