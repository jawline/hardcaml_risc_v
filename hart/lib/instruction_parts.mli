open Hardcaml
open Signal

val opcode : t -> t
val rd : t -> t
val rs1 : t -> t
val rs2 : t -> t
val funct3 : t -> t
val funct7 : t -> t

(** u-immediates are 20-bit immediates encoded at the top of the instruction.
    When decoded, the 20 bits fill the upper 20 bits of the register and the
    remaining bits are filled with zeros. *)
val u_immediate : width:int -> t -> t

(** Sign extended 12-bit immediate stored in the upper 12 bits of the
    instruction. *)
val i_immediate : width:int -> t -> t

(** 12-bit unsigned immediate in the upper 12 bits of the instruction. *)
val csr : width:int -> t -> t

(** S-type immediates are sign extended 12 bit immediates packed into bits
    (7-11) for imm(0:4) and bits 25-31 for bits 5-11 (inclusive). *)
val s_immediate : width:int -> t -> t

(** B-type immediates are sign extended 13 bit immediates packed into bits
    (8-11) for imm(1:4), bits 25-30 for imm(5:10), bit 7 for imm(11) and bit 31
    for imm(12). imm(0) is always 0 (inclusive). *)
val b_immediate : width:int -> t -> t

(** J-type the upper 20 bits of a 21-bit value (sign extended) in the order
    bits 21-30 => imm(1:10), bit 20 => imm(11), bits 12-19 => imm(12:19), bits
    31 => imm(20) (The sign extend bit). *)
val j_immediate : width:int -> t -> t
