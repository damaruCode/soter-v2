use super::ProgLoc;

// Time := ProgLoc^k
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Time<'a> {
    inner: Vec<ProgLoc<'a>>,
}

impl<'a> Time<'a> {
    pub fn new(time: Vec<ProgLoc<'a>>) -> Self {
        Time { inner: time }
    }

    pub fn init() -> Self {
        Time { inner: Vec::new() }
    }

    pub fn tick(&self, prog_loc: ProgLoc<'a>) -> Self {
        let mut ticked_time = self.clone();
        ticked_time.inner.push(prog_loc);
        ticked_time
    }

    pub fn get_contour(&self) -> Vec<ProgLoc<'a>> {
        self.inner.clone()
    }
}
