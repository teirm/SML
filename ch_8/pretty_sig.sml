(* signature for pretty printer *)
signature PRETTY =
    sig
    type expression 
    val blo : int * expression list -> expression
    val str : string -> expression 
    val brk : int -> expression 
    val pr  : TextIO.outstream * expression * int -> unit
    end;
