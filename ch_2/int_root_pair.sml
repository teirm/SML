(* Working through authors solution to 
   int root pair *)

fun introot_pair (n) =
    if n < 4 then (1, n-1)
    else 
    let val (e, re) = introot_pair (n div 4)
        val ri = 4*re + n mod 4
        val rj = ri - (4*e + 1)
    in if rj < 0 then (2*e, ri) else (2*e+1, rj)
    end;
