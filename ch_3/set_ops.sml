(* 
    Set operations in SML
*)


infix mem;
fun (x mem [])      = false
  | (x mem (y::l))  = (x=y) orelse (x mem l);

fun newmem(x,xs) = if x mem xs then xs else x::xs;

fun setof []        = []
  | setof (x::xs)   = newmem(x, setof xs);

fun union ([], ys)      = ys
  | union (x::xs, ys)   = newmem(x, union(xs,ys));

fun inter ([], ys)      = []
  | inter (x::xs, ys)   = if x mem ys then x::inter(xs, ys)
                                      else    inter(xs, ys); 

infix subs;
fun ([]      subs ys) = true
  | ((x::xs) subs ys) = (x mem ys) andalso (xs subs ys);

(* set equality: A U B ^ B U A *)
infix seq;
fun (xs seq ys) = (xs subs ys) andalso (ys subs xs);

fun powset ([], base) = [base]
  | powset (x::xs, base) = 
        powset(xs, base) @ powset(xs, x::base);

fun cartprod ([],    ys) = []
  | cartprod (x::xs, ys) = 
        let val xsprod = cartprod(xs, ys)
            fun pairx  []       = xsprod
              | pairx(y::ytail) = (x,y) :: (pairx ytail)
        in pairx ys end;

fun choose(0, xs, subset, accum) = subset::accum
  | choose(k, [], subset, accum) = accum 
  | choose(k, x::xs, subset, accum) = 
     choose(k-1, xs, x::subset, choose(k, xs, subset, accum));

fun assoc ([], a)               = []
  | assoc ((x,y)::pairs, a)     = if a=x then [y]
                                         else assoc(pairs, a);
