(* Ex 7.30: A functor that takes a structure of signature 
   SEQUENCE and a structure of signature QUEUE and implements
   BFS and DFS
*)

use "../ch_7/queue_sig.sml";
use "../ch_7/sequence_sig.sml";

functor SearchFunctor (structure S  : SEQUENCE
                       structure Q  : QUEUE) =
    struct

    fun depthFirst next x = 
      let fun dfs []      = S.Nil
            | dfs(y::ys)  = S.Cons(y, fn()=>dfs(next y @ ys))
      in dfs[x] end;

    (* list enqueue *)
    fun enql (q, []) = q
      | enql (q, x::xs) = enql(Q.enq(q,x),xs);

    fun breadthFirst next x = 
      let fun bfs q        = if Q.null q then S.Nil
                             else S.Cons(Q.hd(q),fn()=>bfs(enql(Q.deq(q),next(Q.hd(q)))))
      in bfs(Q.enq(Q.empty,x)) end;

    end;
