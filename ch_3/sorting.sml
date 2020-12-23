(*
   sorting operations in SML
*)

local val a = 16807.0 and m = 2147483647.0
in fun nextrand seed = 
        let val t = a*seed 
        in t - m * real(floor(t/m)) end
end;

fun randlist (n, seed, tail) = 
    if n = 0 then (seed,tail)
    else     randlist(n-1, nextrand seed, seed::tail);

(* Insertion Sort *)
fun ins (x, []) : real list = [x]
  | ins (x, y::ys)          = 
        if x <= y then x::y::ys
                  else y::ins(x,ys);

(* Quick Sort *)
fun quick []        = []
  | quick [x]       = [x]
  | quick (a::bs)   = (* pick the head of the list as the pivot *)
        let fun partition (left, right, []): real list =
                (quick left) @ (a :: quick right)
            | partition (left, right, x::xs)          =
                if x<=a then partition (x::left, right, xs)
                        else partition (left, x::right, xs)
        in 
            partition([],[],bs) 
        end;

(* Quicker quick sort -- no append *)
fun quicker ([], sorted)      = sorted 
  | quicker ([x], sorted)     = x::sorted 
  | quicker (a::bs, sorted) = 
        let fun partition (left, right, [], sorted) : real list = quicker(left, a::(quicker(right, sorted)))
              | partition (left, right, x::xs, sorted)          = if x<=a then partition(x::left, right, xs, sorted)
                                                                          else partition(left, x::right, xs, sorted)
        in  
            partition([], [], bs, sorted)
        end;

(* Selection -- study QuickSelect *)
fun quickselect ([], _)         = hd [] (* error -- abort *) 
  | quickselect ([x], _)        = x
  | quickselect (a::bs, i)      =
        let fun partition (left, right, [], i)             = if length(left) + 1 = i then a
                                                             else if length(left) < i 
                                                                  then quickselect(right, i-length(left)-1) 
                                                             else quickselect(left, i)
              | partition (left, right, x::xs, i)          = if x<=a then partition(x::left, right, xs, i)
                                                                     else partition(left, x::right, xs, i)
        in
            partition([], [], bs, i)
        end;

(* merge two lists in sorted order *)
fun merge ([], ys)              = ys : real list
  | merge (xs, [])              = xs
  | merge (x::xs, y::ys)        = 
        if x <= y then x::merge(xs, y::ys)
                  else y::merge(x::xs, ys);

(* top down merge sort *)
fun tmergesort []   = []
  | tmergesort [x]  = [x]
  | tmergesort xs   = 
        let val k = length xs div 2
        in merge(tmergesort(List.take(xs, k)),
                 tmergesort(List.drop(xs,k)))
        end;

fun tmergesort' xs = 
  let fun sort (0, xs)          = ([], xs)
        | sort (1, x::xs)       = ([x], xs)
        | sort (n, xs)          = 
            let val (l1, xs1)   = sort ((n+1) div 2, xs)
                val (l2, xs2)   = sort (n div 2, xs1)
            in (merge (l1, l2), xs2)
            end
    val (l, _) = sort(length xs, xs)
  in l end;

(* bottom up merge sort -- based on O'Keefe 1982 *)
fun mergepairs([l],         k)  = [l]
  | mergepairs(l1::l2::ls,  k)  = 
    if k mod 2 = 1 then l1::l2::ls
    else mergepairs(merge(l1,l2)::ls, k div 2);

fun sorting([],     ls, k) = hd(mergepairs(ls, 0))
  | sorting(x::xs,  ls, k) = 
        sorting(xs, mergepairs([x]::ls, k+1), k+1);

(* smooth applicative merge sort -- sorting on runs, O'Keefe *)
fun nextrun(run, [])        = (rev run, []:real list)
  | nextrun(run, x::xs)     = 
            if x < hd run then (rev run, x::xs)
                          else nextrun(x::xs, xs);

fun samsorting([],  ls, k)      = hd(mergepairs(ls, 0))
  | samsorting(x::xs, ls, k)    = 
      let val (run, tail)   = nextrun([x], xs)
      in samsorting(tail, mergepairs(run::ls, k+1), k+1)
      end;

fun samsort xs = samsorting(xs, [[]], 0);

(* 
    take a list and return 2 lists containing alternating 
    element
*)
fun alts ([],xs,ys)         = (xs, ys)
  | alts ([x],xs,ys)        = (x::xs, ys)
  | alts (x::y::l, xs, ys)  = alts(l, x::xs, y::ys);

(* 
  take n elements put them in 1 list
  take other n elements put them in other
  list
*)
fun takedrop ([], n, xs)    = (xs, [])
  | takedrop (x::l, n, xs)  = 
    if n>0 then takedrop(l, n-1, x::xs)
           else (xs, x::l);
