(* Sequences of infinite lists *)

datatype 'a seq = Nil
                | Cons of 'a * (unit -> 'a seq);

fun hd (Cons(x,xf)) = x
  | hd Nil          = raise Empty;

fun tl (Cons(x,xf)) = xf()
  | tl Nil          = raise Empty;

fun cons (x,xq) = Cons(x, fn()=>xq);

fun from k = Cons(k, fn()=>from(k+1));

fun take (xq, 0)        = [] 
  | take (Nil,n)        = raise Subscript
  | take (Cons(x,xf),n) = x::take(xf(),n-1);

datatype 'a seq2  = Nil2
                  | Cons2 of unit -> 'a * 'a seq2;

fun from2 k = (k, Cons2(fn()=>from2(k+1)));

fun take2 (xq, 0)       = []
  | take2 (Nil2, n)     = raise Subscript
  | take2 (Cons2(xf),n)  = 
        let val (v,xq) = xf()
        in v :: take2(xq, n-1)
        end;

datatype 'a seqnode = Nil3
                    | Cons3 of 'a * 'a seq2
and      'a seq2     = Seq of unit -> 'a seqnode;

fun from3 k = Cons3(k, Seq(fn()=>from3(k+1)));

fun take3 (xq, 0)                = []
  | take3 (Nil3, n)              = raise Subscript
  | take3 (Cons3(x,Seq(xf)),n)   = x::take3(xf(),n-1);

fun squares Nil : int seq       = Nil
  | squares (Cons(x,xf))        = Cons(x*x, fn()=>squares(xf()));

fun add (Cons(x,xf), Cons(y, yf))   = Cons(x+y, fn()=>add(xf(), yf()))
  | add _                           = Nil;

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

fun null Nil = true
  | null _   = false;

fun add_pairs Nil : int seq            = Nil
  | add_pairs (Cons(x,xf))             =
        (case xf() of
            Nil             => Cons(x+0, fn()=>add_pairs(Nil))
          | (Cons(y,yf))    => Cons(x+y, fn() => add_pairs(yf())));
