(*
  Graph operations in SML
*)

use "set_ops.sml";

(* find all successors of a node *)
fun nexts (a, [])               = []
  | nexts (a, (x,y)::pairs)     = 
          if (a=x) then y::nexts(a, pairs)
          else             nexts(a, pairs);

(* DFS *)
fun depthf ([], graph, visited)     = rev visited
  | depthf (x::xs, graph, visited)  = 
        if x mem visited then depthf(xs, graph, visited)
        else depthf(nexts(x,graph) @ xs, graph, x::visited);

(* DFS *)
fun depth ([], graph, visited) = rev visited
  | depth (x::xs, graph, visited) = 
      depth(xs, graph, 
            if x mem visited then visited
            else depth(nexts(x,graph), graph, x::visited));

fun topsort graph = 
  let fun sort ([], visited)    = visited
        | sort (x::xs, visited) = 
            sort(xs, if x mem visited then visited
                     else x :: sort(nexts(x,graph), visited))
      val (starts,_) = ListPair.unzip graph (* ('a * 'b) list -> 'a list * 'b list *)
  in 
      sort(starts, [])
  end; 

fun pathsort graph = 
  let fun sort ([], path, visited)      = visited
        | sort (x::xs, path, visited)   = 
             if x mem path then hd[] (* abort *)
             else sort(xs, path, 
                       if x mem visited then visited else
                       x :: sort(nexts(x,graph), x::path, visited))
      val (starts, _) = ListPair.unzip graph
  in 
      sort(starts, [], []) 
  end;

fun newvisit (x, (visited, cys)) = (x::visited, cys);

fun cyclesort graph = 
    let fun sort ([],       path,   (visited, cys)) = (visited, cys)
          | sort (x::xs,    path,   (visited, cys)) =
              sort(xs, path,
                   if x mem path            then (visited, x::cys)
                   else if x mem visited    then (visited, cys)
                   else newvisit(x, sort(nexts(x,graph),
                                        x::path, (visited,cys))))
        val (starts, _) = ListPair.unzip graph
    in 
        sort(starts, [], ([],[])) 
    end;
