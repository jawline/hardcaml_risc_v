open Hardcaml

module type S = sig
  type 'a t =
    { pc : 'a
    ; general : 'a list
    }
  [@@deriving hardcaml]

  val set_pc : 'a t -> 'a -> 'a t

  val assign_when
    :  when_:Signal.t
    -> index_signal:Signal.t
    -> value_signal:Signal.t
    -> Signal.t t
    -> Signal.t t

  module For_writeback : sig
    type nonrec 'a registers = 'a t

    type 'a t =
      { pc : 'a
      ; general : 'a list
      }
    [@@deriving hardcaml]

    val to_registers : Signal.t t -> Signal.t registers
    val of_registers : 'a registers -> 'a t
  end
end
