(* Purpose gcd function
   Author: Cyrus Ramavarapu *)

fun gcd(m, n) = if m = n then m
            else if m mod 2 = 0 andalso n mod 2 = 0 then 2 * gcd(m div 2, n div 2)
            else if m mod 2 = 0 andalso n mod 2 = 1 then gcd(m div 2, n)
            else gcd((((n div 2) - 1) - ((m div 2) - 1)), m);  
