(* signature for versioned arrays *)
signature VARRAY =
    sig
    type 'a t
    val array       : int * 'a -> 'a t
    val reroot      : 'a t -> 'a t
    val sub         : 'a t * int -> 'a
    val justUpdate  : 'a t * int * 'a -> 'a t
    val update      : 'a t * int * 'a -> 'a t
    end;
