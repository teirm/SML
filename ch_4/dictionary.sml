(* Dictionary operations in SML *)

use "trees.sml";

signature DICTIONARY = 
    sig 
    type key
    type 'a t
    exception E of key
    val empty   : 'a t
    val lookup  : 'a t * key -> 'a
    val insert  : 'a t * key * 'a -> 'a t
    val update  : 'a t * key * 'a -> 'a t
    end;

structure Dict : DICTIONARY =
    struct

    type key    = string;
    type 'a t   = (key * 'a) tree;

    exception E of key;

    val empty = Lf;

    fun lookup (Lf, b)                      = raise E b
      | lookup (Br ((a,x), t1, t2), b)      = 
        (case String.compare(a,b) of 
            GREATER => lookup(t1, b)
          | EQUAL   => x
          | LESS    => lookup(t2, b));

    fun insert (Lf,b,y)                   = Br((b,y), Lf, Lf)
      | insert (Br((a,x),t1,t2),b,y)       = 
        (case String.compare(a,b) of
            GREATER => Br((a,x), insert(t1,b,y),t2)
          | EQUAL   => raise E b
          | LESS    => Br((a,x),t1, insert(t2,b,y)));

    fun update (Lf,b,y)                     = Br((b,y), Lf, Lf)
      | update (Br((a,x),t1,t2),b,y)        = 
            (case String.compare(a,b) of  
                GREATER => Br((a,x),update(t1,b,y),t2)
              | EQUAL   => Br((a,y),t1,t2)
              | LESS    => Br((a,x),t1,update(t2,b,y)));

    end;

structure ListDict : DICTIONARY =
    struct

    type key        = string;
    type 'a t       = (key * 'a) list;

    exception E of key;

    val empty = [];

    fun lookup ([], b)              = raise E b
      | lookup ((k,v)::xs, b)       =
        (case String.compare(k,b) of 
            EQUAL   => v
          | _       => lookup(xs, b));

    fun insert ([],b,y)                 = [(b,y)]
      | insert ((k,v)::xs,b,y)          = 
        (case String.compare(k,b) of 
            GREATER => (b,y)::(k,v)::xs
          | EQUAL   => raise E b
          | LESS    => (k,v)::insert(xs,b,y));

    fun update ([],b,y)                 = [(b,y)]
      | update ((k,v)::xs,b,y)          = 
        (case String.compare(k,b) of
            GREATER => (b,y)::(k,v)::xs
          | EQUAL   => (k,y)::xs
          | LESS    => (k,v)::update(xs,b,y));
    end;
