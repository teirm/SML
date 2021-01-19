(* 
   A sorting functor providing quick sort and 
   merge sort 
*)

use "../ch_7/order_sig.sml";

functor Sorting (Item : ORDER) = 
    struct
    structure Item = Item;
    
    fun x <= y = (Item.compare(x,y) <> GREATER);

    fun quick([],  sorted)     = sorted
      | quick([x], sorted)   = x::sorted
      | quick(a::bs, sorted) = 
            let fun partition(left, right, [], sorted) = 
                        quick(left, a::(quick(right, sorted)))
                  | partition(left, right, x::xs, sorted) =
                    if x <= a then partition(x::left, right, xs, sorted)
                              else partition(left, x::right, xs, sorted);
            in 
                partition([], [], bs, sorted)
            end;

    fun merge ([], ys)          = ys : Item.t list 
      | merge (xs, [])          = xs
      | merge (x::xs, y::ys)    = 
            if x <= y then x::merge(xs, y::ys)
                      else y::merge(x::xs, ys);
    
    fun tmergesort []   = []    : Item.t list
      | tmergesort [x]  = [x]
      | tmergesort xs   = 
            let val k = length xs div 2
            in merge(tmergesort(List.take(xs, k)),
                     tmergesort(List.drop(xs,k)))
            end;

end;
