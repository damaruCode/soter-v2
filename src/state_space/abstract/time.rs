// Time := ProgLoc^k
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Time {
    pub inner: Vec<usize>,
}

impl Time {
    pub fn new(time: Vec<usize>) -> Self {
        Time { inner: time }
    }

    pub fn init() -> Self {
        Time { inner: Vec::new() }
    }

    pub fn tick(&self, prog_loc: usize) -> Self {
        let mut ticked_time = self.clone();
        ticked_time.inner.push(prog_loc);
        ticked_time
    }
}
