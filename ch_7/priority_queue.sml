(* A functor for priority queues *)

use "../ch_7/pq_sig.sml";

functor PriorityQueue (Item: ORDER) : PRIORITY_QUEUE =
    struct
    structure Item = Item;

    fun x <= y = (Item.compare(x,y) <> GREATER);

    abstype t = Leaf
              | Bran of Item.t * t * t
        with

        val empty = Leaf;

        fun null Leaf       = true
          | null _          = false;
        
        fun siftdown (w : Item.t, Leaf, Leaf)              = Bran(w,Leaf,Leaf)
          | siftdown (w, t as Bran(v, Leaf, Leaf), Leaf)   = 
                if w <= v then Bran(w, t, Leaf)
                          else Bran(v, Bran(w, Leaf, Leaf), Leaf)
          | siftdown(w, t1 as Bran(v1,p1,q1), t2 as Bran(v2, p2, q2)) = 
                if w <= v1 andalso w <= v2 then Bran(w, t1, t2)
                else if v1 <= v2 then Bran(v1, siftdown(w,p1,q1), t2)
                                 else Bran(v2, t1, siftdown(w, p2, q2));
       
        fun leftrem (Bran(v, Leaf, Leaf))     = (v, Leaf)
          | leftrem (Bran(v, t1, t2))         = 
                let val (w, t) = leftrem t1
                in (w, Bran(v, t2, t)) end;

        fun min (Bran(v, _, _))   = v
          | min Leaf              = raise Size;

        fun insert (w : Item.t, Leaf)       = Bran(w, Leaf, Leaf)
          | insert (w, Bran(v, t1, t2))  =
                if w <= v then Bran(w, insert(v, t2), t1)
                          else Bran(w, insert(w, t2), t1);
        
        fun delmin Leaf                     = raise Size
          | delmin (Bran(v, Leaf, _))       = Leaf
          | delmin (Bran(v, t1, t2))        = 
                let val (w,t) = leftrem t1
                in siftdown (w, t2, t1) end;
       
        fun heapify (n, [])         = raise Size
          | heapify (0, vs)         = (Leaf, vs)
          | heapify (n, v::vs)      = 
                let val (t1, vs1)   = heapify (n div 2, vs)
                    val (t2, vs2)   = heapify ((n-1) div 2, vs1)
                in (siftdown (v, t1, t2), vs2) end;

        fun fromList vs = #1 (heapify(length vs, vs)); 
        

        fun toList (t as Bran(v,_,_))     = v :: toList(delmin t)
          | toList Leaf                     = [];
        

        fun sort vs = toList (fromList vs);
    end
end;
