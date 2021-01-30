(* Working with references in SML *)

fun +:= (id, E) = id := !id + E;
