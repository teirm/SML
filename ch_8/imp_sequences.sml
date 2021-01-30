(* sequence implementation using an impe ML style *)

use "../ch_8/imp_sequence_sig.sml";

structure ImpSeq :> IMP_SEQUENCE = 
    struct
    datatype 'a t = Nil
                  | Cons    of 'a * ('a t) ref  
                  | Delayed of unit -> 'a t
    
    exception Empty

    fun delay xf = ref(Delayed xf);

    val empty = Nil;

    fun cons(x, xf) = Cons(x, delay xf);

    fun force xp = 
        case !xp of 
            Delayed f => let val s = f()
                         in xp := s; s end
          | s => s;

    fun null Nil        = true
      | null (Cons _)   = false;

    fun hd Nil          = raise Empty
      | hd (Cons(x, _)) = x;

    fun tl Nil              = raise Empty
      | tl (Cons(_, xp))    = force xp;
    
    fun take (xq, 0)            = []
      | take (Nil, n)           = []
      | take (Cons(x,xp), n)    = x :: take(force xp, n-1);

    fun          Nil @ yq = yq
      | (Cons(x,xp)) @ yq = 
            Cons(x, delay(fn() => (force xp) @ yq));

    fun map f Nil           = Nil
      | map f (Cons(x,xp))  = 
            Cons(f x, delay(fn() => map f (force xp)));
    
    fun fromList []      = Nil
      | fromList (x::xs) = cons(x, fn()=>fromList(xs));  

    fun toList Nil              = []
      | toList (Cons(x,xp))     = x::toList(force(xp))
    
    fun interleave (Nil, yq)        = yq
      | interleave (Cons(x,xf),yq)  = 
            Cons(x, delay(fn()=>interleave(yq, force(xf))));

    fun filter pred Nil             = Nil
      | filter pred (Cons(x,xp))    = 
            if pred x then Cons(x, delay(fn()=>filter pred (force(xp))))
                      else filter pred (force(xp));
    
    (* This takes a sequence of sequences and concatenates them *) 
    fun concat xqq =  
        if null xqq then empty
        else if null(hd xqq) then concat(tl xqq)
             else cons(hd (hd xqq),
                       fn() => tl (hd xqq) @ concat (tl xqq));

    (* This works because:
       initially knot references Nil -- then we 
       set knot to reference to seqfn that then dereferences
       knot, but each time !knot is updated
    *)
    fun cycle seqfn =
        let val knot = ref Nil
        in knot := seqfn (fn() => !knot); !knot end;
    
    end;

