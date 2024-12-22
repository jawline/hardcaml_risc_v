#[derive(Debug)]
pub struct Rand {
    state: u32,
}

impl Rand {
    pub fn new() -> Self {
        Self { state: 13923919 }
    }
    pub fn next(&mut self) -> u32 {
        let c = self.state;
        let c = c ^ (c << 13);
        let c = c ^ (c >> 17);
        let c = c ^ (c << 5);
        self.state = c;
        c
    }
}
