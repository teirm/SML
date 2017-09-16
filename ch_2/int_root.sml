(* Improving on the introot with let *)

fun increase(k,n) = if (k+1)*(k+1) > n then k else k +1
fun introot n = if n=0 then 0 else increase(2 * introot (n div 4), n)
