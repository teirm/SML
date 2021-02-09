(* type signature *)
signature TYPE =
    sig
    datatype t = Con of string * t list | Var of string
    val pr   : t -> unit
    val read : string -> t
    end;
