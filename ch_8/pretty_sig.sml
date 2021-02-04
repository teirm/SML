(* signature for pretty printer *)
signature PRETTY =
    sig
    type t
    val blo : int * t list -> t
    val str : string -> t
    val brk : int -> t
    val pr  : TextIO.outstream * t * int -> unit
    end;
