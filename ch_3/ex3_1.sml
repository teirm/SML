use "list_ops.sml";

fun maxl xs = if null (tl xs) then hd xs
              else 
                let val a = hd xs
                    val b = hd (tl xs)
                in if (a > b) then maxl (a::tl(tl xs))
                   else maxl (b::tl(tl xs))
                end;
