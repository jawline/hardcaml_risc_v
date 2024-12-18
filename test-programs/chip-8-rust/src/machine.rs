use crate::cpu::Cpu;
use crate::memory::{Framebuffer, Memory, RawMemory};

/// The CHIP-8 ran at roughly ~500Hz and clocks tick at 60Hhz, so we should tick the clocks
/// roughly 8 times per step
pub const CLOCKS_PER_DELAY: usize = 8;

pub struct Machine {
    pub cpu: Cpu,
    pub memory: Memory,
    clocks_since_delay: usize,
}

impl Machine {
    /// Create a new machine with empty memory
    #[allow(dead_code)]
    pub fn new(memory: RawMemory, framebuffer: Framebuffer) -> Self {
        Self {
            cpu: Cpu::new(),
            memory: Memory::new(memory, framebuffer),
            clocks_since_delay: 0,
        }
    }

    /// Set the machine key to the given state and clear the wait_for_key register if necessary
    pub fn _set_key(&mut self, key: u8, state: bool) {
        let current = self.cpu.registers.keys[key as usize];

        if state && !current {
            if let Some(register) = self.cpu.registers.wait_for_key {
                self.cpu.registers.v[register] = key;
                self.cpu.registers.wait_for_key = None;
            }
        }

        self.cpu.registers.keys[key as usize] = state;
    }

    /// Return true if the device should currently be playing sound
    pub fn _sound(&self) -> bool {
        self.cpu.registers.sound > 0
    }

    /// Step the machine, this steps the CPU and decrements the delay and sound timers when
    /// appropriate
    pub fn step(&mut self) {
        // Only step the CPU if we are not waiting for a key press
        if let None = self.cpu.registers.wait_for_key {
            self.cpu.step(&mut self.memory);
        }

        // Increment the timers at roughly 1 clock per 8 steps
        self.clocks_since_delay += 1;

        if self.clocks_since_delay >= CLOCKS_PER_DELAY {
            if self.cpu.registers.sound > 0 {
                self.cpu.registers.sound -= 1;
            }

            if self.cpu.registers.delay > 0 {
                self.cpu.registers.delay -= 1;
            }
        }
    }
}
