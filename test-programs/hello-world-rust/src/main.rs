#![no_std]
#![no_main]
mod util;

use core::{arch::global_asm, panic::PanicInfo};
use util::send_dma_l;

global_asm!(include_str!("entry.s"));

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    send_dma_l("PANIC");
    loop {}
}

#[no_mangle]
extern "C" fn main() -> () {
    send_dma_l("HELLO WORLD");
    send_dma_l("AND NOW GOODBYE");
    loop {}
}
