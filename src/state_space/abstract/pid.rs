use super::{ProgLoc, Time};

// Pid := ProgLoc x Time
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Pid<'a> {
    prog_loc: ProgLoc<'a>,
    time: Time<'a>,
}

impl<'a> Pid<'a> {
    pub fn new(&self, prog_loc: ProgLoc<'a>, time: Time<'a>) -> Self {
        // Generated the Pid for the new ProcState using its ProgLoc and Time
        let mut vec = time.get_contour();
        // NOTE not sure this is the proper way to do this anymore
        vec.append(&mut self.time.tick(self.prog_loc.clone()).get_contour());

        Pid {
            prog_loc,
            time: Time::new(vec),
        }
    }

    pub fn init(prog_loc: ProgLoc<'a>) -> Self {
        Pid {
            prog_loc,
            time: Time::init(),
        }
    }
}
