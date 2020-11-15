(* Exercise 2.6
   Author: Cyrus Ramavarapu
   Purpose: Compare times
*)

fun minute_compare(m1, m2) = 
    if m1 > m2 then 1
    else if m1 < m2 then ~1
    else (* m1 == m2 *) 0;

fun hour_compare((h1,m1),(h2,m2)) =
    if h1 > h2 then ~1
    else if h1 < h2 then 1 
    else minute_compare(m1, m2);

fun time_compare((h1,m1,e1), (h2,m2,e2)) =
    if e1="AM" andalso e2="AM" orelse e1="PM" andalso e2="PM" then hour_compare((h1,m1),(h2,m2))
    else if e1="AM" andalso e2="PM" then 1
    else (* e1="PM" andalso e2="AM" *) ~1;


        


