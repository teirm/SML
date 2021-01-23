(* functor for queues *)

use "../ch_7/queue_sig.sml";

functor QueueFunctor () : QUEUE =
    struct 
    datatype 'a t = Queue of ('a list * 'a list)
    exception E;

    val empty = Queue([],[]);

    fun norm (Queue([], tails)) = Queue(rev tails, [])
      | norm q                  = q;

    fun enq(Queue(heads,tails), x) = norm(Queue(heads, x::tails));

    fun null(Queue([],[]))  = true
      | null _              = false;

    fun hd(Queue(x::_,_))   = x
      | hd(Queue([],_))      = raise E;
    
    fun deq(Queue(x::heads,tails)) = norm(Queue(heads,tails))
      | deq(Queue([],_))           = raise E;


    fun length(Queue(heads,tails)) = List.length(heads) + List.length(tails);

    fun equal((Queue(h1,t1)),(Queue(h2,t2))) = (h1=h2 andalso t1=t2);
    end;
