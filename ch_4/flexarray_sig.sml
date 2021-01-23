(* a signature for flexible arrays *)

signature FLEXARRAY = 
    sig 
    type 'a array
    val empty   : 'a array
    val length  : 'a array -> int
    val sub     : 'a array * int -> 'a
    val update  : 'a array * int * 'a -> 'a array
    val loext   : 'a array * 'a -> 'a array
    val lorem   : 'a array -> 'a array
    val hiext   : 'a array * 'a -> 'a array
    val hirem   : 'a array -> 'a array
    end;
