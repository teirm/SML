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
                    | Cons3 of 'a * 'a seq
and      'a seq     = Seq of unit -> 'a seqnode;

fun from3 k = Cons3(k, Seq(fn()=>from3(k+1)));
