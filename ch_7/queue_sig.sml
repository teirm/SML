(* A signature for queues *)

signature QUEUE = 
    sig 
    eqtype 'a t                         (* type of queues *)
    exception E                         (* for errors in hd, deq *)
    val empty  : 'a t                   (* the empty queue *)
    val enq    : 'a t * 'a -> 'a t      (* add to end *)
    val null   : 'a t -> bool           (* test for empty queue *)
    val hd     : 'a t -> 'a             (* return front element *)
    val deq    : 'a t -> 'a t           (* remove from front *)
    val length : 'a t -> int            (* return number of items in queue *)
    val equal  : ''a t * ''a t -> bool  (* determine if two queues are equal *)
    end;
