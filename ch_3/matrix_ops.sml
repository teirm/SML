(*
    Matrix operations in SML
*)

fun headcol []                  = []
  | headcol ((x::_) :: rows)    = x :: headcol rows;

fun tailcols []                 = []
  | tailcols ((_::xs) :: rows)  = xs :: tailcols rows;

fun transp ([]::rows)           = []
  | transp rows                 = headcol rows :: transp (tailcols rows);
