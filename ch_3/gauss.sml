(*
  Gaussian Elimination in SML
*)

fun pivotrow [row]              = row : real list
  | pivotrow (row1::row2::rows) = 
    if abs(hd row1) >= abs(hd row2) 
    then pivotrow(row1::rows)
    else pivotrow(row2::rows);

fun delrow (p, [])              = []
  | delrow (p, row::rows)       = if p = hd row then rows
                                  else row :: delrow(p, rows);

fun scalarprod (k, [])           = []
  | scalarprod (k, x::xs)        = k*x :: scalarprod(k, xs);

fun vectorsum ([], [])           = [] : real list
  | vectorsum (x::xs, y::ys)     = (x+y) :: vectorsum(xs, ys);

fun gausselim [row] = [row]
  | gausselim rows = 
    let val p::prow = pivotrow rows
        fun elimcol []
          | elimcol ((x::xs)::rows) =
              vectorsum(xs, scalarprod(~x/p, prow))
              :: elimcol rows
    in (p::prow) :: gausselim(elimcol(delrow(p, rows)))
    end;

fun solutions
