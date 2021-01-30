(* Signature for ring buffer in SML *)

signature RINGBUF = 
    sig 
    eqtype 'a t
    exception Empty
    val empty       : unit -> 'a t
    val null        : 'a t -> bool
    val label       : 'a t -> 'a
    val moveLeft    : 'a t -> unit
    val moveRight   : 'a t * 'a -> unit
    val delete      : 'a t -> 'a
    end;
