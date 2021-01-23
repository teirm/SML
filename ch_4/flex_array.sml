(* Flexible arrays and Braun trees *)

use "../ch_4/trees.sml";

(* subscript the flexible array and retrieve the entry *)
fun sub (Lf, _)         = raise Subscript
  | sub (Br(v,t1,t2),k) = 
        if k = 1 then v
        else if k mod 2 = 0
             then sub(t1, k div 2)
             else sub(t2, k div 2);

(* update the entry at position k with value w *)
fun update (Lf, k, w) =
        if k = 1 then Br(w, Lf, Lf)
        else raise Subscript
  | update (Br(v,t1,t2), k, w) =
        if k = 1 then Br(w, t1, t2)
        else if k mod 2 = 0
             then Br(v, update(t1, k div 2, w), t2)
             else Br(v, t1, update(t2, k div 2, w));

(* delete the entry at n in the array *)
fun delete (Lf, n)          = raise Subscript
  | delete (Br(v,t1,t2),n)  = 
          if n = 1 then Lf
          else if n mod 2 = 0
               then Br(v, delete(t1,n div 2), t2)
               else Br(v, t1, delete(t2, n div 2));

(* extend the tree with w and pushing everything down *)
fun loext (Lf, w)           = Br(w,Lf,Lf)
  | loext (Br(v,t1,t2),w)   = Br(w,loext(t2,v),t1);

(* remove entry at the end *)
fun lorem (Lf)                          = raise Size
  | lorem (Br(_,Lf,Lf))                 = Lf
  | lorem (Br(_,t1 as Br(v,_,_),t2))    = Br(v,t2,lorem t1);

fun fill_flex_array (x, n)  = compsame(x, n);

fun to_list (Lf, v)                     = v
  | to_list (t as Br(a,_, _), v)        = to_list(lorem(t), a::v); 
