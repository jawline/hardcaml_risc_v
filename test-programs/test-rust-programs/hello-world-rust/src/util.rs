extern "C" {
    pub fn system_call(code: u32, msg: *const u8, len: u32) -> bool;
}

pub fn send_dma_l(s: &str) {
    unsafe { while !system_call(0, s.as_ptr(), s.len() as u32) {} }
}
