(* Generic dictionary in SML *)

(* 
   refering from '..' roots the use call from the top dir -- others who use 
   this file are similarly rooted...need to learn some imperative IO before
   writing something that will search the project for it >:3
*)
use "../ch_7/order_sig.sml";
use "../ch_4/dictionary_sig.sml";

(* Need types with an ordering for dictionaries *)
structure StringOrder : ORDER =
    struct
    type t = string;
    val compare = String.compare
    end;

functor Dictionary (Key: ORDER) : DICTIONARY =
    struct
   
    type key = Key.t;

    abstype 'a t = Leaf
                 | Bran of key * 'a * 'a t * 'a t
        with 
        
        exception E of key;

        val empty = Leaf;

        fun lookup (Leaf, b)            = raise E b
          | lookup (Bran(a,x,t1,t2), b)  = 
                (case Key.compare(a,b) of 
                        GREATER => lookup(t1, b)
                    |   EQUAL   => x
                    |   LESS    => lookup(t2, b));

        fun insert (Leaf, b, y)             = Bran(b, y, Leaf, Leaf)
          | insert (Bran(a,x,t1,t2), b, y)  = 
                (case Key.compare(a,b) of 
                        GREATER => Bran(a, x, insert(t1, b, y), t2)
                    |   EQUAL   => raise E b
                    |   LESS    => Bran(a, x, t1, insert(t2, b, y)));

        fun update (Leaf, b, y)             = Bran(b, y, Leaf, Leaf)
          | update (Bran(a,x,t1,t2), b, y)  = 
                (case Key.compare(a,b) of
                        GREATER => Bran(a, x, update(t1, b, y), t2)
                    |   EQUAL   => Bran(a, y, t1, t2)
                    |   LESS    => Bran(a, x, t1, update(t1, b, y)));
    end
end;

(* 
   Representation of a Dictionary using Lists 
   In my opinion -- uglier and less efficient 
   than the tree repsentation 
*)
functor ListDictionary (Key : ORDER) : DICTIONARY = 
    struct 

    type key = Key.t;

    abstype 'a t = LD of (key * 'a) list
        with

        exception E of key;

        val empty = LD([]);

        fun lookup (LD [], b)            = raise E b
          | lookup (LD((k,v)::xs), b)    = 
                (case Key.compare(k,b) of
                    EQUAL => v
                 |  ord   => lookup(LD(xs),b));
        
        fun insert (LD [], b, y)           = LD([(b,y)])
          | insert (LD((k,v)::xs), b, y)    = 
                (case Key.compare(k,b) of 
                        GREATER => LD((b,y)::(k,v)::xs)
                    |   EQUAL   => raise E b
                    |   LESS    => insert(LD(xs), b, y));

        fun update (LD [], b, y)           = LD([(b,y)])
          | update (LD((k,v)::xs), b, y)    = 
                (case Key.compare(k,b) of 
                      GREATER => LD((b,y)::(k,v)::xs)
                    | EQUAL   => LD((b,y)::xs)
                    | LESS    => update(LD(xs), b, y));
    end
end;
