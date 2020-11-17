(* 
  Exercise 3.2
  Function to rerturn last element of a 
  list.
*)

fun last (m::[]) = m
  | last (_::ms) = last(ms);
