#![no_std]
#![no_main]
mod cpu;
mod machine;
mod memory;

use core::{arch::global_asm, panic::PanicInfo};

global_asm!(include_str!("entry.s"));

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[no_mangle]
extern "C" fn main() -> () {
    loop {}
}
