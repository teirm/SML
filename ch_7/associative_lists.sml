(* Associative Lists in SML *)

use "../ch_4/dictionary_sig.sml";
use "../ch_7/order_sig.sml";

functor AssocList (eqtype key) : DICTIONARY =
    struct
    type key    = key;
    type 'a t   = (key * 'a) list;

    exception E of key;

    val empty = [];

    fun lookup((a,x)::pairs, b)     = if a=b then x
                                             else lookup(pairs, b)
      | lookup([], b)               = raise E b;

    fun insert ((a,x)::pairs, b, y) = if a=b then raise E b
                                             else (a,x)::insert(pairs, b, y)
      | insert ([], b, y)           = [(b,y)];

    fun update(pairs, b, y)         = (b,y)::pairs;

    end;

functor OrderedAssocList (structure Key : ORDER) : DICTIONARY =
    struct

    type key = Key.t;

    abstype 'a t = OAL of (key * 'a) list 
        with

        exception E of key;

        val empty = OAL([]);

        fun lookup (OAL [], b)              = raise E b
          | lookup (OAL((a,x)::pairs), b)   = if Key.compare(a,b) = EQUAL then x
                                                                        else lookup(OAL(pairs),b);
        
        fun insert (OAL((a,x)::pairs), b, y) = if Key.compare(a,b)=EQUAL then raise E b
                                                     else let val OAL(res) = insert(OAL pairs, b,y)
                                                          in OAL((a,x)::res) end
          | insert (OAL([]), b, y)                = OAL([(b,y)]);

        fun update(OAL(pairs), b, y)        = OAL((b,y)::pairs);
        end;
    end;
