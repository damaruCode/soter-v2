use super::Time;

type ProgLoc = usize;

// Pid := ProgLoc x Time
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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
