(* Exercise 2.6
   Author: Cyrus Ramavarapu
   Purpose: Subtract triplets of money*)

fun subtract_money((p1,s1,n1),(p2,s2,n2)) = check_negatives((p1-p2, s1-s2, n1-n2));


