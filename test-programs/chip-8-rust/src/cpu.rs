use crate::memory::Memory;
use core::num::Wrapping;

/// Size of an instruction (CHIP-8 uses fixed width opcodes)
pub const INSTRUCTION_SIZE: u16 = 0x2;

/// If opcode has the form _XN_ or _XR_ then the first register can be extracted with this mask
pub const REGISTER_MASK: u16 = 0x0F00;

/// If the opcode has the form _XR_ the second register can be extracted with this mask
pub const REGISTER_TWO_MASK: u16 = 0x00F0;

/// If opcodes have the form __II then the immediate value can be extracted with this mask
pub const DATA_MASK: u16 = 0x00FF;

/// If the opcode immediate contaisn only a single nibble of data (the final nibble of the opcode)
/// we extract it with this mask
pub const NIBBLE_DATA_MASK: u16 = 0x000F;

/// The number of key registers
pub const NUM_KEYS: usize = 16;

#[derive(Debug)]
pub struct Registers {
    /// The CHIP architecture has 16 8-bit general purpose registers.
    /// Register v[f] also doubles as the carry flag, collision flag, or borrow flag dependent on
    /// the operation.
    pub v: [Wrapping<u8>; 16],
    /// The program counter
    pub pc: Wrapping<u16>,
    /// The address register
    pub i: Wrapping<u16>,

    /// The stack is only used for return
    pub stack: [Wrapping<u8>; 256],
    pub stack_idx: usize,

    /// The delay timer counts down to zero at 60hz
    pub delay: Wrapping<u8>,

    /// The sound timer emits a sound if it is not zero.
    /// This timer counts down to zero at 60hz and then stops.
    pub sound: Wrapping<u8>,

    /// True if a given key is currently pressed
    pub keys: [bool; NUM_KEYS],

    /// If we are waiting for a key then this is Some of the register to write the key to
    /// otherwise None
    pub wait_for_key: Option<usize>,
}

pub struct OpTables {
    pub main_op_table: [Instruction; 16],
    pub math_op_table: [Instruction; 9],
    pub load_op_table: [Instruction; 0x66],
}

impl Registers {
    /// Increment the PC by a given amount
    pub fn inc_pc(&mut self, val: u16) {
        self.pc += Wrapping(val);
    }

    /// Push a u16 to the stack in big-endian format
    pub fn stack_push16(&mut self, value: u16) {
        let lower_part = Wrapping((value & 0x00FF) as u8);
        let upper_part = Wrapping(((value & 0xFF00) >> 8) as u8);
        self.stack[self.stack_idx] = upper_part;
        self.stack[self.stack_idx + 1] = lower_part;
        self.stack_idx += 2;
    }

    /// Pop a u16 from the stack
    /// TODO: Since stack is only ever used for retcodes I could just keep them as usize or u16's
    pub fn stack_pop16(&mut self) -> u16 {
        self.stack_idx -= 2;
        let upper_part = self.stack[self.stack_idx];
        let lower_part = self.stack[self.stack_idx + 1];

        ((upper_part.0 as u16) << 8) | (lower_part.0 as u16)
    }
}

#[derive(Clone, Copy)]
pub struct Instruction {
    /// Execute the opcode, with the change in state being reflected in registers and memory
    pub execute:
        fn(registers: &mut Registers, memory: &mut Memory, data: u16, op_tables: &OpTables),
}

impl Instruction {
    /// The zero opcode can be either clear display, ret, or machine call (Call an instruction
    /// written in machine code) depending on parameters. We merge these all into one opcode
    /// execution.
    fn mcall_display_or_flow(
        registers: &mut Registers,
        memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        match data {
            0xE0 => {
                memory.clear_display();
                registers.inc_pc(2);
            }
            0xEE => {
                let new_pc = registers.stack_pop16();
                registers.pc = Wrapping(new_pc);
            }
            _ => panic!("machine code routes are unsupported {:x}", data),
        }
    }

    /// Goto changes the PC pointer to the fixed location
    fn goto(registers: &mut Registers, _memory: &mut Memory, data: u16, _op_tables: &OpTables) {
        registers.pc = Wrapping(data);
    }

    /// Call pushes a return address and then changes I to the given location
    fn call(registers: &mut Registers, _memory: &mut Memory, data: u16, _op_tables: &OpTables) {
        // First save the current PC + 2
        registers.stack_push16(registers.pc.0 + INSTRUCTION_SIZE);

        // Jump to the immediate
        registers.pc = Wrapping(data);
    }

    /// Extract the register from the opcode when the instruction has the form _R__
    fn register_from_data(data: u16) -> u8 {
        ((data & REGISTER_MASK) >> 8) as u8
    }

    /// Extract the register from the opcode when the register has the form __R_
    fn register_two_from_data(data: u16) -> u8 {
        ((data & REGISTER_TWO_MASK) >> 4) as u8
    }

    /// Extract the immediate from the opcode when the instruction has the form __II
    fn immediate_from_data(data: u16) -> u8 {
        (data & DATA_MASK) as u8
    }

    /// Extract both the register and immediate for instructions in the form _RII
    fn register_and_immediate_from_data(data: u16) -> (usize, u8) {
        (
            Self::register_from_data(data) as usize,
            Self::immediate_from_data(data),
        )
    }

    /// Extract two registers from and opcode in the form _RV_
    fn two_registers_from_data(data: u16) -> (usize, usize) {
        (
            Self::register_from_data(data) as usize,
            Self::register_two_from_data(data) as usize,
        )
    }

    /// Checks if a register and an immediate value are equal. If they are equal then we
    /// skip the next instruction, otherwise we run the next instruction.
    fn reg_equal(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register, data) = Self::register_and_immediate_from_data(data);
        registers.inc_pc(if registers.v[register as usize] == Wrapping(data) {
            4
        } else {
            2
        });
    }

    /// Checks if a register and an immediate are not equal. If they are not equal then skip the
    /// next instruction, otherwise run the next instruction.
    fn reg_not_equal(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register, data) = Self::register_and_immediate_from_data(data);
        registers.inc_pc(if registers.v[register as usize] != Wrapping(data) {
            4
        } else {
            2
        });
    }

    /// Checks if two registers are equal. If they are then skip the next instruction, otherwise
    /// run it.
    fn two_reg_equal(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, register2) = Self::two_registers_from_data(data);
        registers.inc_pc(if registers.v[register1] == registers.v[register2] {
            4
        } else {
            2
        });
    }

    /// Load an immediate into a register
    fn load_immediate(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register, data) = Self::register_and_immediate_from_data(data);
        registers.v[register] = Wrapping(data);
        registers.inc_pc(2);
    }

    /// Same as load immediate but add it to the register rather than add
    fn add_immediate(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register, data) = Self::register_and_immediate_from_data(data);
        registers.v[register] = registers.v[register] + Wrapping(data);
        registers.inc_pc(2);
    }

    /// The math or bitops instruction picks a opcode from the math_opcode table
    /// using the final nibble as it's value
    fn math_or_bitop(
        registers: &mut Registers,
        memory: &mut Memory,
        data: u16,
        op_tables: &OpTables,
    ) {
        let math_opcode = data & NIBBLE_DATA_MASK;
        (op_tables.math_op_table[math_opcode as usize].execute)(registers, memory, data, op_tables);
    }

    /// Test if two registers are not equal. If they are not equal then skip the next instruction,
    /// otherwise run it.
    fn two_registers_not_equal(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, register2) = Self::two_registers_from_data(data);
        registers.inc_pc(if registers.v[register1] != registers.v[register2] {
            4
        } else {
            2
        });
    }

    /// Set the I register to an immediate value
    fn set_i(registers: &mut Registers, _memory: &mut Memory, data: u16, _op_tables: &OpTables) {
        registers.i = Wrapping(data);
        registers.inc_pc(2);
    }

    /// Jump to an immediate value plus the value of V[0]
    fn jump_immediate_plus_register(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        registers.pc = Wrapping(registers.v[0].0 as u16) + Wrapping(data);
    }

    /// The masked random instruction generates a random value between 0 and 255, masks it with an
    /// immediate (& imm) and then places it in a specified register.
    fn masked_random(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register, mask) = Self::register_and_immediate_from_data(data);
        panic!("RNG");
        //let rval = 0;
        //registers.v[register].0 = rval & mask;
        //registers.inc_pc(2);
    }

    /// Draw a sprite from memory to the framebuffer (which is stored in the Memory structure).
    /// 0x0F00 is the X position, 0x00F0 is the Y position and 0x000F is the depth of the sprite.
    /// The sprite is drawn downward using the data at i, incrementing y by 1 for every pixel down
    /// it goes.
    fn draw_sprite(
        registers: &mut Registers,
        memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, register2) = Self::two_registers_from_data(data);
        let d = data & NIBBLE_DATA_MASK;
        registers.v[0xF] = Wrapping(memory.draw_sprite(
            registers.v[register1].0 as usize,
            registers.v[register2].0 as usize,
            d as usize,
            registers.i.0 as usize,
        ));
        registers.inc_pc(2);
    }

    /// If the final byte = 0x9E then skip the next instruction if key[register[data & 0x0F00]] is
    /// pressed.
    /// If the final byte = 0xA1 then skip the next instruction if key[register[data & 0x0F00]] is
    /// not pressed
    fn key_op(registers: &mut Registers, _memory: &mut Memory, data: u16, _op_tables: &OpTables) {
        let (register1, _register2) = Self::two_registers_from_data(data);
        let rval = registers.v[register1];
        let pressed = registers.keys[rval.0 as usize];
        let code = data & 0x00FF;

        match code {
            0x9E => {
                if pressed {
                    registers.inc_pc(4);
                } else {
                    registers.inc_pc(2);
                }
            }
            0xA1 => {
                if pressed {
                    registers.inc_pc(2);
                } else {
                    registers.inc_pc(4);
                }
            }
            _ => panic!("unexpected keyop {}", code),
        };
    }

    fn load_or_store(
        registers: &mut Registers,
        memory: &mut Memory,
        data: u16,
        op_tables: &OpTables,
    ) {
        let opcode_mask = data & 0x00FF;
        (op_tables.load_op_table[opcode_mask as usize].execute)(registers, memory, data, op_tables);
    }

    fn mv_register(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, register2) = Self::two_registers_from_data(data);
        registers.v[register1] = registers.v[register2];
        registers.inc_pc(2);
    }

    fn or_register(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, register2) = Self::two_registers_from_data(data);
        registers.v[register1] |= registers.v[register2];
        registers.inc_pc(2);
    }

    fn and_register(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, register2) = Self::two_registers_from_data(data);
        registers.v[register1] &= registers.v[register2];
        registers.inc_pc(2);
    }

    fn xor_register(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, register2) = Self::two_registers_from_data(data);
        registers.v[register1] ^= registers.v[register2];
        registers.inc_pc(2);
    }

    fn add_register(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, register2) = Self::two_registers_from_data(data);
        let result = registers.v[register1] + registers.v[register2];

        registers.v[0xF] = if result < registers.v[register1] {
            Wrapping(1)
        } else {
            Wrapping(0)
        };

        registers.v[register1] = result;

        registers.inc_pc(2);
    }

    fn sub_register(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, register2) = Self::two_registers_from_data(data);
        let result = registers.v[register1] - registers.v[register2];

        registers.v[0xF] = if result > registers.v[register1] {
            Wrapping(1)
        } else {
            Wrapping(0)
        };

        registers.v[register1] = result;

        registers.inc_pc(2);
    }

    fn shr_register(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, _register2) = Self::two_registers_from_data(data);
        registers.v[0xF].0 = registers.v[register1].0 & 0x1;
        registers.v[register1].0 >>= 1;
        registers.inc_pc(2);
    }

    fn shl_register(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, _register2) = Self::two_registers_from_data(data);
        registers.v[0xF].0 = registers.v[register1].0 & (0x1 << 7);
        registers.v[register1].0 <<= 1;
        registers.inc_pc(2);
    }

    fn rev_sub_register(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, register2) = Self::two_registers_from_data(data);
        let result = registers.v[register2] - registers.v[register1];

        registers.v[0xF] = if result > registers.v[register2] {
            Wrapping(1)
        } else {
            Wrapping(0)
        };

        registers.v[register1] = result;

        registers.inc_pc(2);
    }

    fn invalid_op(
        _registers: &mut Registers,
        _memory: &mut Memory,
        _data: u16,
        _op_tables: &OpTables,
    ) {
        panic!("invalid");
    }

    fn get_delay(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, _register2) = Self::two_registers_from_data(data);
        registers.v[register1] = registers.delay;
        registers.inc_pc(2);
    }

    fn set_delay(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, _register2) = Self::two_registers_from_data(data);
        registers.delay = registers.v[register1];
        registers.inc_pc(2);
    }

    fn set_sound(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, _register2) = Self::two_registers_from_data(data);
        registers.sound = registers.v[register1];
        registers.inc_pc(2);
    }

    fn wait_for_key(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, _register2) = Self::two_registers_from_data(data);
        registers.wait_for_key = Some(register1);
        registers.inc_pc(2);
    }

    fn add_vx_i(registers: &mut Registers, _memory: &mut Memory, data: u16, _op_tables: &OpTables) {
        let (register1, _register2) = Self::two_registers_from_data(data);
        registers.i += Wrapping(registers.v[register1].0 as u16);
        registers.inc_pc(2);
    }

    fn set_i_sprite_addr(
        registers: &mut Registers,
        _memory: &mut Memory,
        data: u16,
        _op_tables: &OpTables,
    ) {
        let (register1, _register2) = Self::two_registers_from_data(data);
        registers.i.0 = 0x4000 + ((registers.v[register1].0 & 0x0F) as u16 * 5);
        registers.inc_pc(2);
    }

    fn bcd_vx(registers: &mut Registers, memory: &mut Memory, data: u16, _op_tables: &OpTables) {
        let (register1, _) = Self::two_registers_from_data(data);
        let mut tmp = registers.v[register1];

        // Least significant digit
        memory.set((registers.i + Wrapping(2)).0 as usize, tmp % Wrapping(10));
        tmp /= Wrapping(10);

        // Middle digit
        memory.set((registers.i + Wrapping(1)).0 as usize, tmp % Wrapping(10));
        tmp /= Wrapping(10);

        // Most significant digit
        memory.set(registers.i.0 as usize, tmp % Wrapping(10));

        registers.i += Wrapping(3);
        registers.inc_pc(2);
    }

    fn reg_dump(registers: &mut Registers, memory: &mut Memory, data: u16, _op_tables: &OpTables) {
        let (register1, _) = Self::two_registers_from_data(data);
        for i in 0..(register1 as usize + 1) {
            memory.set(registers.i.0 as usize, registers.v[i]);
            registers.i += Wrapping(1);
        }
        registers.inc_pc(2);
    }

    fn reg_load(registers: &mut Registers, memory: &mut Memory, data: u16, _op_tables: &OpTables) {
        let (register1, _) = Self::two_registers_from_data(data);
        for i in 0..(register1 as usize + 1) {
            registers.v[i] = memory.get(registers.i.0 as usize);
            registers.i += Wrapping(1);
        }
        registers.inc_pc(2);
    }

    pub fn load_op_table() -> [Self; 0x66] {
        let mut load_op_table: [Self; 0x66] = [Self {
            execute: Self::invalid_op,
        }; 0x66];

        load_op_table[0x07] = Self {
            execute: Self::get_delay,
        };

        load_op_table[0x0A] = Self {
            execute: Self::wait_for_key,
        };

        load_op_table[0x15] = Self {
            execute: Self::set_delay,
        };

        load_op_table[0x18] = Self {
            execute: Self::set_sound,
        };

        load_op_table[0x1E] = Self {
            execute: Self::add_vx_i,
        };

        load_op_table[0x29] = Self {
            execute: Self::set_i_sprite_addr,
        };

        load_op_table[0x33] = Self {
            execute: Self::bcd_vx,
        };

        load_op_table[0x55] = Self {
            execute: Self::reg_dump,
        };

        load_op_table[0x65] = Self {
            execute: Self::reg_load,
        };

        load_op_table
    }

    pub fn math_op_table() -> [Self; 9] {
        let mv = Self {
            execute: Self::mv_register,
        };

        let or = Self {
            execute: Self::or_register,
        };

        let and = Self {
            execute: Self::and_register,
        };

        let xor = Self {
            execute: Self::xor_register,
        };

        let add = Self {
            execute: Self::add_register,
        };

        let sub = Self {
            execute: Self::sub_register,
        };

        let shr = Self {
            execute: Self::shr_register,
        };

        let rsub = Self {
            execute: Self::rev_sub_register,
        };

        let shl = Self {
            execute: Self::shl_register,
        };

        [mv, or, and, xor, add, sub, shr, rsub, shl]
    }

    pub fn main_op_table() -> [Self; 16] {
        let mcall_instruction = Self {
            execute: Self::mcall_display_or_flow,
        };

        let goto_instruction = Self {
            execute: Self::goto,
        };

        let call_instruction = Self {
            execute: Self::call,
        };

        let reg_eq = Self {
            execute: Self::reg_equal,
        };

        let reg_neq = Self {
            execute: Self::reg_not_equal,
        };

        let two_reg_eq = Self {
            execute: Self::two_reg_equal,
        };

        let load_immediate = Self {
            execute: Self::load_immediate,
        };

        let add_immediate = Self {
            execute: Self::add_immediate,
        };

        let math_or_bitop = Self {
            execute: Self::math_or_bitop,
        };

        let two_reg_not_equal = Self {
            execute: Self::two_registers_not_equal,
        };

        let set_i = Self {
            execute: Self::set_i,
        };

        let jump_imm_plus_register = Self {
            execute: Self::jump_immediate_plus_register,
        };

        let masked_random = Self {
            execute: Self::masked_random,
        };

        let draw_sprite = Self {
            execute: Self::draw_sprite,
        };

        let key_op = Self {
            execute: Self::key_op,
        };

        let load_or_store = Self {
            execute: Self::load_or_store,
        };

        [
            mcall_instruction,
            goto_instruction,
            call_instruction,
            reg_eq,
            reg_neq,
            two_reg_eq,
            load_immediate,
            add_immediate,
            math_or_bitop,
            two_reg_not_equal,
            set_i,
            jump_imm_plus_register,
            masked_random,
            draw_sprite,
            key_op,
            load_or_store,
        ]
    }
}

/// The CPU holds the current program registers and the instruction op tables
pub struct Cpu {
    pub registers: Registers,
    pub op_tables: OpTables,
}

impl Cpu {
    /// Create a fresh CPU instance with 0 / false set for all registers and PC set to 0x200 (the
    /// typical ROM start location)
    pub fn new() -> Self {
        Self {
            registers: Registers {
                pc: Wrapping(0x200),
                v: [Wrapping(0); 16],
                i: Wrapping(0),
                stack: [Wrapping(0); 256],
                stack_idx: 0,
                delay: Wrapping(0),
                sound: Wrapping(0),
                keys: [false; NUM_KEYS],
                wait_for_key: None,
            },
            op_tables: OpTables {
                main_op_table: Instruction::main_op_table(),
                math_op_table: Instruction::math_op_table(),
                load_op_table: Instruction::load_op_table(),
            },
        }
    }

    pub fn step(&mut self, memory: &mut Memory) {
        let next_opcode = memory.get16(self.registers.pc.0 as usize).0;
        let op_id = ((next_opcode & 0xF000) >> 12) as usize;
        (self.op_tables.main_op_table[op_id].execute)(
            &mut self.registers,
            memory,
            next_opcode & 0x0FFF,
            &self.op_tables,
        );
    }
}
