use super::Time;

// Pid := ProgLoc x Time
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Pid {
    prog_loc: usize,
    time: Time,
}

impl Pid {
    pub fn new(&self, prog_loc: usize, time: Time) -> Self {
        // Generated the Pid for the new ProcState using its ProgLoc and Time
        let mut vec = time.inner;
        // NOTE not sure this is the proper way to do this anymore
        vec.append(&mut self.time.tick(self.prog_loc).inner);

        Pid {
            prog_loc,
            time: Time::new(vec),
        }
    }

    pub fn init() -> Self {
        Pid {
            prog_loc: 0,
            time: Time::init(),
        }
    }
}
