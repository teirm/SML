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
                                                             else if length(left) <= i then quickselect(right, i-length(left)-1) 
                                                             else quickselect(left, i)
              | partition (left, right, x::xs, i)          = if x<=a then partition(x::left, right, xs, i)
                                                                     else partition(left, x::right, xs, i)
        in
            partition([], [], bs, i)
        end;
