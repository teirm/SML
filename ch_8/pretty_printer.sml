(* Implementation of Pretty Printer in SML
   inspired by Oppen 1980
*)

use "../ch_8/pretty_sig.sml";

structure Pretty : PRETTY =
    struct
    datatype    t = Block   of t list * int * int
                  | String  of string
                  | Break   of int;
   
    (* Given a list of t compute length of all values + after ignoring breaks *)
    fun breakdist (Block(_,_,len)::es, after)   = len + breakdist(es, after)
      | breakdist (String s :: es, after)       = size s + breakdist(es, after)
      | breakdist (Break _ :: es, after)        = 0
      | breakdist ([], after)                   = after;

    (* length function for type t *)
    fun length (Block(_,_,len)) = len
      | length (String s)       = size s
      | length (Break len)      = len;
    
    val str = String and brk = Break;

    fun blo (indent, es) = 
        let fun sum ([],    k) = k
              | sum (e::es, k) = sum(es, length e + k)
        in Block(es, indent, sum(es, 0)) end;
    
    fun pr (os, e, margin) =
        let val space = ref margin
            
            fun blanks n = (TextIO.output(os, StringCvt.padLeft #" " n "");
                            space := !space - n)
            
            fun newLine() = (TextIO.output(os, "\n"); space := margin)

            fun printing ([], _, _)                    = ()
              | printing (e::es, blockspace, after)    = 
                (case e of 
                      Block(bes, indent, len) => 
                        printing(bes, !space-indent, breakdist(es,after))
                    | String s  => (TextIO.output(os,s); space := !space - size s)
                    | Break len => 
                        if len + breakdist(es,after) <= !space
                        then blanks len
                        else (newLine(); blanks(margin-blockspace));
                    printing(es,blockspace,after))
        in printing([e],margin,0); newLine() end;
    end;
