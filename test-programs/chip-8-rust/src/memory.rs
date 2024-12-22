pub type Framebuffer = *mut [u8; SCREEN_SIZE];
pub type RawMemory = *mut [u8; MEMORY_SIZE];

/// The CHIP-8 VM has 4kb of user accessible memory
pub const MEMORY_SIZE: usize = 1024 * 8;

pub const SCREEN_WIDTH: usize = 64;
pub const SCREEN_HEIGHT: usize = 32;
pub const SCREEN_SIZE: usize = SCREEN_WIDTH * SCREEN_HEIGHT;

/// The CHIP-8 VM has sprites for the characters 0-F hardcoded. These bytes encode that.
pub const SPRITE_MEM: [u8; 5 * 16] = [
    0xF0_u8, 0x90, 0x90, 0x90, 0xF0, 0x20, 0x60, 0x20, 0x20, 0x70, 0xF0, 0x10, 0xF0, 0x80, 0xF0,
    0xF0, 0x10, 0xF0, 0x10, 0xF0, 0x90, 0x90, 0xF0, 0x10, 0x10, 0xF0, 0x80, 0xF0, 0x10, 0xF0, 0xF0,
    0x80, 0xF0, 0x90, 0xF0, 0xF0, 0x10, 0x20, 0x40, 0x40, 0xF0, 0x90, 0xF0, 0x90, 0xF0, 0xF0, 0x90,
    0xF0, 0x10, 0xF0, 0xF0, 0x90, 0xF0, 0x90, 0x90, 0xE0, 0x90, 0xE0, 0x90, 0xE0, 0xF0, 0x80, 0x80,
    0x80, 0xF0, 0xE0, 0x90, 0x90, 0x90, 0xE0, 0xF0, 0x80, 0xF0, 0x80, 0xF0, 0xF0, 0x80, 0xF0, 0x80,
    0x80,
];

/// The memory structure contains the user accessible data and the current frame buffer.
pub struct Memory {
    memory: RawMemory,
    pub frame_buffer: Framebuffer,
}

impl Memory {
    /// Create a new completely clear memory
    pub fn new(memory: RawMemory, frame_buffer: Framebuffer) -> Self {
        Self {
            memory,
            frame_buffer,
        }
    }

    /// Get a u8 from memory. If the address is > 0x4000 then it references the SPRITE_MEM
    /// containing text
    pub fn get(&self, idx: usize) -> u8 {
        unsafe {
            if idx < 0x4000 {
                (*self.memory)[idx]
            } else {
                SPRITE_MEM[idx - 0x4000]
            }
        }
    }

    /// Set a u8 in memory
    pub fn set(&mut self, idx: usize, val: u8) {
        unsafe {
            (*self.memory)[idx] = val;
        }
    }

    /// Return a u16 in system order from memory, performing necessary endianness conversion
    pub fn get16(&self, idx: usize) -> u16 {
        let first_part = self.get(idx);
        let second_part = self.get(idx + 1);
        let combined = first_part as u16 | (second_part as u16) << 8;
        u16::from_be(combined)
    }

    /// Clear the entire framebuffer
    pub fn clear_display(&mut self) {
        for i in 0..SCREEN_SIZE {
            unsafe {
                (*self.frame_buffer)[i] = 0;
            }
        }
    }

    pub fn draw_sprite(&mut self, x: usize, y: usize, n: usize, i: usize) -> u8 {
        let mut vf_reg = 0;

        for yoff in 0..n {
            let y = (y + yoff) % SCREEN_HEIGHT;
            let sprite = self.get(i + yoff);

            for xoff in 0..8 {
                let x = (x + xoff) % SCREEN_WIDTH;

                unsafe {
                    let fb = &mut (*self.frame_buffer);
                    let fb_idx = (y * SCREEN_WIDTH) + x;
                    let byte_index = fb_idx >> 3;
                    let bit_index = fb_idx & 0b111;
                    // TODO: Return 1 if any pixel touched is already set. Flip it then also
                    let xor_value = if sprite & (1 << (7 - xoff)) != 0 {
                        1
                    } else {
                        0
                    };

                    let current_byte = fb[byte_index];
                    let current_bit = (current_byte >> bit_index) & 1;
                    let new_value = current_bit ^ xor_value;

                    if current_bit == 1 && new_value == 0 {
                        vf_reg = 1;
                    }

                    fb[byte_index] = (current_byte & !(1 << bit_index)) | new_value << bit_index;
                };
            }
        }

        vf_reg
    }
}
