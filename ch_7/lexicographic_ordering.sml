(* 
    A functor with two structures defining
    a lexicographic ordering on them:

     Let <a be an ordering on type a
     Let <b be an ordering on type b 

     then <(axb) on type a x b is 

     ('a,'b) <(axb) (a,b) iff 'a <a a 
     or 'a = a and 'b <b b.
*)

use "../ch_7/order_sig.sml";

functor LexOrder (structure O1: ORDER 
                  structure O2: ORDER) : ORDER =

    struct
    type t = O1.t * O2.t;
    fun compare ((x1,y1), (x2,y2)) = 
        (case O1.compare(x1,x2) of 
            EQUAL => O2.compare(y1,y2)
          | ord   => ord)
    end;
