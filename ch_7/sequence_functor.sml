(* Functor implementing the signature of SEQUENCE *)

use "../ch_7/sequence_sig.sml";

functor LazyList () :> SEQUENCE =
    struct 
    
    exception Empty;

    datatype 'a seq = Nil | Cons of 'a * (unit -> 'a seq)

    fun cons (x,xq) = Cons(x, fn()=>xq);
    
    fun null Nil = true
      | null _   = false;

    fun hd (Cons(x,xf)) = x
      | hd Nil          = raise Empty;

    fun tl (Cons(x,xf)) = xf()
      | tl Nil          = raise Empty;
    
    fun fromList l = List.foldr cons Nil l;
    
    fun toList Nil              = []
      | toList (Cons(x,xf))   = x::toList(xf());

    fun take (xq, 0)        = [] 
      | take (Nil,n)        = raise Subscript
      | take (Cons(x,xf),n) = x::take(xf(),n-1);

    fun drop (xq, 0)        = xq
      | drop (Nil,n)        = raise Subscript
      | drop (Cons(x,xf),n) = drop(xf(),n-1);

    fun Nil             @ yq    = yq
      | (Cons(x,xf))    @ yq    = Cons(x,fn() => (xf()) @ yq);
    
    fun interleave (Nil, yq)        = yq
      | interleave(Cons(x,xf),yq)   = 
            Cons(x,fn()=>interleave(yq, xf()));

    fun map f Nil                   = Nil
      | map f (Cons(x, xf))          = Cons(f x, fn() => map f (xf()));

    fun filter pred Nil             = Nil
      | filter pred (Cons(x,xf))    =
            if pred x then Cons(x, fn() => filter pred (xf()))
                      else filter pred (xf());

    fun iterates f x = Cons(x, fn() => iterates f (f x));

    fun from k = Cons(k, fn()=>from(k+1));
    end;
