(* Three representations of a Queue in SML *)

structure Queue1 = 
    struct 

    type 'a t = 'a list;
    exception E;

    val empty = [];

    fun enq(q,x)    = q @ [x];
    
    fun null(x::q)  = false
      | null _      = true;

    fun hd(x::q)    = x
      | hd []       = raise E;

    fun deq(x::q)   = q
      | deq []      = raise E;
    end;

structure Queue2 = 
    struct 

    datatype 'a t = empty
                  | enq of 'a t * 'a;

    exception E;

    fun null (enq _)   = false
      | null empty     = true;

    fun hd (enq(empty,x))   = x
      | hd (enq(q,x))       = hd q
      | hd empty            = raise E;

    fun deq (enq(empty,x))  = empty
      | deq (enq(q,x))      = enq(deq q, x)
      | deq empty           = raise E;
    end;


(* store a queue as a reversed list
   so [x1,x2,...,xn] represents queue 
   xn,...,x2,x1
*)
structure Queue2a = 
    struct
    type 'a t = 'a list;
    exception E;
    
    val empty = [];

    fun enq(q,x)    = x::q;

    fun null(x::q)  = false
      | null _      = true;

    fun hd([x])     = x
      | hd(x::q)    = hd(q)
      | hd([])      = raise E;
    
    fun deq([])     = raise E
      | deq(q)      = 
        let fun deq_int(accum,[])   = raise E
              | deq_int(accum,[x])  = rev accum
              | deq_int(accum,x::q) = deq_int(x::accum, q)
        in deq_int([], q) end;
    end;

(* Burton, 1982 - Store a queue as a a pair of lists 
   ([x1,x2,....,xN],[y1,y2,...,yM])
  is the quque 
    x1x2....xNyM.....,y1,y2
*)
structure Queue3 =
    struct 
    datatype 'a t = Queue of ('a list * 'a list);
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
    end;
