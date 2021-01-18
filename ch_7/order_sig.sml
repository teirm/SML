(* Signature for ordered types *)

signature ORDER = 
    sig 
    type t
    val compare: t*t -> order
    end;

