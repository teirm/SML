(*
    Matrix operations in SML
*)

use "list_ops.sml";

fun headcol []                  = []
  | headcol ((x::_) :: rows)    = x :: headcol rows;

fun tailcols []                 = []
  | tailcols ((_::xs) :: rows)  = xs :: tailcols rows;

fun transp ([]::rows)           = []
  | transp rows                 = headcol rows :: transp (tailcols rows);

fun dotprod ([], [])            = 0.0
  | dotprod (x::xs, y::ys)      = x*y + dotprod(xs,ys);

fun rowprod (row, [])           = []
  | rowprod (row, col::cols)    = dotprod(row,col) :: rowprod(row,cols);

fun rowlistprod ([], cols)          = []
  | rowlistprod (row::rows, cols)   = 
      rowprod(row,cols) :: rowlistprod(rows,cols);

fun matprod(rowsA, rowsB) = rowlistprod(rowsA, transp rowsB);

fun rowneg ([])                 = []
  | rowneg (x::xs)              = ~x :: rowneg(xs);

fun matrixneg ([])               = []
  | matrixneg (row::rows)        = rowneg(row) :: matrixneg(rows);

fun add_rows ([], [])            = []
  | add_rows (x::xs, y::ys)      = x+y :: add_rows(xs, ys);

fun matrix_add ([], [])          = []
  | matrix_add (rowA::rowsA, rowB::rowsB) = 
      add_rows(rowA, rowB) :: matrix_add(rowsA, rowsB);

fun change_row_signs ([])           = []
  | change_row_signs ([a])          = [a]
  | change_row_signs (a::b::xs)     = a::(~b::change_row_signs(xs));

fun remove_column ([], p)           = []
  | remove_column (row::rows, p)    = remove_by_index(row, p)::remove_column(rows,p); 

fun shrink_matrix (row::rows, p)    = remove_column(rows,p);

