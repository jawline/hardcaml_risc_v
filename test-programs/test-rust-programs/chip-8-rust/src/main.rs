#![no_std]
#![no_main]
mod cpu;
mod machine;
mod memory;
mod rand;
mod util;

use core::{arch::global_asm, panic::PanicInfo};
use machine::Machine;
use memory::{Framebuffer, RawMemory};
use util::send_dma_l;

global_asm!(include_str!("entry.s"));

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    send_dma_l("PANIC");

    if let Some(location) = info.location() {
        send_dma_l(location.file());
        let mut buffer = itoa::Buffer::new();
        send_dma_l(buffer.format(location.line()));
        let mut buffer = itoa::Buffer::new();
        send_dma_l(buffer.format(location.column()));
    } else {
        send_dma_l("No location");
    }

    if let Some(message) = info.message().as_str() {
        send_dma_l(message);
    } else {
        send_dma_l("Cannot to_str msg");
    }

    loop {}
}

#[unsafe(no_mangle)]
extern "C" fn main() -> () {
    send_dma_l("Starting up");
    let program: &[u8; 4096] = include_bytes!("space_invaders_padded.ch8");
    let program = program.as_ptr() as RawMemory;

    // Print out the program location in the ROM
    send_dma_l("Transmitting program location");
    let mut buffer = itoa::Buffer::new();
    send_dma_l(buffer.format(program as usize));

    send_dma_l("Got handle on program");
    let frame_buffer: Framebuffer = (0x8000) as Framebuffer;
    send_dma_l("Initialized framebuffer");
    let mut machine = Machine::new(program, frame_buffer);
    send_dma_l("Initialized, stepping");
    loop {
        machine.step();
        for _i in 0..5000 {
            unsafe {
                let x = 0x9000 as *mut u8;
                core::ptr::write_volatile(x, 0);
            }
        }
    }
}
