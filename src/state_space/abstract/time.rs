type ProgLoc = usize;

// Time := ProgLoc^k
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Time {
    pub inner: Vec<ProgLoc>,
}

impl Time {
    pub fn init() -> Self {
        Time { inner: Vec::new() }
    }

    pub fn tick(&self, prog_loc: usize) -> Self {
        let mut ticked_time = self.clone();
        ticked_time.inner.push(prog_loc);
        ticked_time
    }

    pub fn append(&mut self, mut vec: Vec<ProgLoc>) {
        self.inner.append(&mut vec);
    }
}
