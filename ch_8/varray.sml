(* versioned array in SML *)

use "../ch_8/varray_sig.sml";

structure Varray :> VARRAY =
    struct 
    datatype 'a t = Modif of {limit : int,
                              index : int ref,
                              elem  : 'a ref,
                              next  : 'a t ref}
                |   Main of 'a Array.array;

    fun array (n, x) =
        if n < 0 then raise Size
        else Modif{limit=n, index=ref 0, elem=ref x,
                   next=ref(Main(Array.array(n,x)))};
    
    fun reroot (va as Modif{index, elem, next,...}) = 
        case !next of 
            Main    _ => va (* reached the root *)
          | Modif   _ => 
                let val Modif{index=bindex,elem=belem,next=bnext,...} =
                                reroot(!next)
                    val Main ary = !bnext
                in bindex := !index;
                   belem  := Array.sub(ary, !index);
                   Array.update(ary, !index, !elem);
                   next   := !bnext;
                   bnext  := va;
                   va
                end;

    fun sub (Modif{index,elem,next,...},i) =
        case !next of 
            Main ary => Array.sub(ary,i)
          | Modif _  => if !index = i then !elem
                                      else sub(!next,i);

    fun justUpdate (va as Modif{limit,...}, i, x) =
        if 0<=i andalso i<limit
        then Modif{limit=limit, index=ref i, elem=ref x, next=ref va}
        else raise Subscript;

    fun update(va,i,x) = reroot(justUpdate(va,i,x));
    end;
