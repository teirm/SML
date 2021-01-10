(* Some search algorithms using infinite lists *)

use "sequences.sml";

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
