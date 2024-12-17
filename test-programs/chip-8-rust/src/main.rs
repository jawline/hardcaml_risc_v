#![no_std]
#![no_main]
mod cpu;
mod machine;
mod memory;
mod util;

use core::{arch::global_asm, panic::PanicInfo};
use machine::Machine;
use memory::Framebuffer;
use util::send_dma_l;

global_asm!(include_str!("entry.s"));

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    send_dma_l("PANIC");
    loop {}
}

#[no_mangle]
extern "C" fn main() -> () {
    send_dma_l("Starting up");
    let program = include_bytes!("space_invaders.ch8");
    let frame_buffer: Framebuffer = (0x8000) as Framebuffer;
    let mut machine = Machine::of_bytes(frame_buffer, program);

    send_dma_l("Initialized, stepping");
    loop {
        machine.step();
    }
}
