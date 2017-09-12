(* The newton raphson method  for square roots.
   Solves a - x^2 = 0 *)

fun sqrt a =
    let acc = 10E~10 
        fun findroot x =
            let val nextx = (a/x + x) / 2.0
            in if abs (x - nextx) < acc*x
                then nextx else findroot (a, nextx, acc)
            end;
    in findroot 1.0
