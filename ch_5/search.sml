(* Some search algorithms using infinite lists *)

use "sequences.sml";
use "../ch_3/set_ops.sml";
use "../ch_3/list_ops.sml";

fun depthFirst next x =
    let fun dfs []      = Nil
          | dfs(y::ys)  = Cons(y, fn()=>dfs(next y @ ys))
    in dfs [x] end;

fun breadthFirst next x = 
    let fun bfs[]       = Nil
          | bfs(y::ys)  = Cons(y, fn()=>bfs(ys @ next y))
    in bfs [x] end;

fun depthFirstUntil next pred x =
    let fun dfs []      = Nil
          | dfs(y::ys)  = if pred(y) then Cons(y, fn()=>dfs(next y @ ys))
                                     else dfs(next y @ ys)
    in dfs [x] end;

fun breadthFirstUntil next pred x = 
    let fun bfs[]       = Nil
          | bfs(y::ys)  = if pred(y) then Cons(y, fn()=>bfs(ys @ next y))
                                     else bfs(ys @ next y)
    in bfs [x] end;

(* Palindromes *)
fun nextChar l = [#"A"::l, #"B"::l, #"C"::l];

fun isPalin l = (l = rev l);

fun show n csq = map implode (Seq.take(csq,n))

(* 8-Queens Problem:
   Thinking of it as a tree with the root an empty board
   and each child the board with a queen in one of n possible
   locations, and then each grand child with another possibility
   is a good way to think about it
*)
fun safeQueen oldqs newq = 
    let fun nodiag (i, [])     = true
          | nodiag (i, q::qs)  = 
                Int.abs(newq-q)<>i andalso nodiag(i+1, qs)
    in not (newq mem oldqs) andalso nodiag(1, oldqs) end;

fun nextQueen n qs = 
  List.map (secr op:: qs) (List.filter (safeQueen qs) (upto(1,n)))

fun isFull n qs = (length qs=n);

fun depthQueen n = Seq.filter (isFull n) (depthFirst (nextQueen n) []);

(* Iterative Deepening *)
fun depthIter next x = 
    let fun dfs k (y, sf) = 
        if k=0 then fn()=>Cons(y,sf)
        else foldr (dfs (k-1)) sf (next y)
    fun deepen k = dfs k (x, fn()=>deepen(k+1)) ()
    in deepen 0 end;
