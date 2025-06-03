open! Core

module Make (Hart_config : Hart_config_intf.S) (Registers : Registers_intf.S) :
  Decoded_instruction_intf.M(Registers).S
