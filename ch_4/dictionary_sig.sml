(* A signature for dictionaries *)

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
