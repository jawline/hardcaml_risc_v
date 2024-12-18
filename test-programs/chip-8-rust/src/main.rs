#![no_std]
#![no_main]
mod cpu;
mod machine;
mod memory;
mod util;

use core::{arch::global_asm, panic::PanicInfo};
use machine::Machine;
use memory::{Framebuffer, RawMemory};
use util::send_dma_l;

global_asm!(include_str!("entry.s"));

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    send_dma_l("PANIC");
    send_dma_l(info.message().as_str().unwrap());
    loop {}
}

#[unsafe(no_mangle)]
extern "C" fn main() -> () {
    send_dma_l("Starting up");
    send_dma_l("Starting up 2");
    // TODO: This should not work without resizing to MemorySize
    let program = include_bytes!("space_invaders.ch8").as_ptr() as RawMemory;
    send_dma_l("Got handle on program");
    let frame_buffer: Framebuffer = (0x8000) as Framebuffer;
    send_dma_l("Initialized framebuffer");
    let mut machine = Machine::new(program, frame_buffer);
    send_dma_l("Initialized, stepping");
    loop {
        send_dma_l("Step");
        machine.step();
    }
}
