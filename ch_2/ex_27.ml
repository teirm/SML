(* Exercise 2.6
   Author: Cyrus Ramavarapu
   Purpose: Subtract triplets of money*)

fun check_negatives (x,y,z) = if x >= 0 andalso y >= 0 andalso z >= 0 then (x,y,z)
                              else if x < 0 then (0,0,0)
                              else if x > 0 andalso y > 0 andalso z < 0 then check_negatives(x,y-1,z+10)
                              else if x > 0 andalso y < 0 andalso z > 0 then check_negatives(x-1,y+10,z)
                              else (* x > 0 andalso y < 0 andalso z < 0 *) check_negatives(x-1,y+9,z+10)


fun subtract_money((p1,s1,n1),(p2,s2,n2)) = check_negatives((p1-p2, s1-s2, n1-n2));


