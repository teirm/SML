(* An attempt at a more efficient int root
   program -- based off of newton - raphson *)

fun int_root2 a = find_root (a, 1); 

fun find_root (a, x) =
    let val nextx = (a div x + x) div 2
    in if abs (x - nextx) <= 1 
        then nextx else find_root(a, nextx)
    end;

