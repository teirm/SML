(* Implementation of Pretty Printer in SML
   inspired by Oppen 1980
*)

use "../ch_8/pretty_sig.sml";

structure Pretty : PRETTY =
    struct
    
    (*  3 kinds of expression:
            Block contains a list of ts (Block, String, Break), indent size, and current length
            String is string
            Break  is of a specific length
    *)
    datatype expression = Block   of expression list * int * int
                        | String  of string
                        | Break   of int;
   
    (* Given a list of t compute length of all values + after ignoring breaks *)
    fun breakdist (Block(_,_,len)::exprs, after)   = len + breakdist(exprs, after)
      | breakdist (String s :: exprs, after)       = size s + breakdist(exprs, after)
      | breakdist (Break _ :: exprs, after)        = 0
      | breakdist ([], after)                      = after;

    (* length function for expressions *)
    fun length (Block(_,_,len)) = len
      | length (String s)       = size s
      | length (Break len)      = len;

    (* simplified constructors for expressions *)
    val str = String and brk = Break;

    (* Create a block containing the given expressions but 
       compute the total length of all expressions in the 
       given list. Indent size is fixed.

       Inner function sum just computes the total length 
       given expression list.
    *)
    fun blo (indent, exprs) = 
        let fun sum ([],      k) = k
              | sum (e::es, k) = sum(es, length e + k)
        in Block(exprs, indent, sum(exprs, 0)) end;
   
    (* Print out a pretty structure given an expression:
      
       os           is the output stream
       e            is an expression of datatype expression
       margin       right margin
     *)
    fun pr (os, e, margin) =
        let val space = ref margin
            
            (* Insert n blanks and set the space to space - # blanks *)
            fun blanks n = (TextIO.output(os, StringCvt.padLeft #" " n "");
                            space := !space - n)
            
            (* Insert a new line and reset space to margin *)
            fun newLine() = (TextIO.output(os, "\n"); space := margin)

            (* print the individual expressions in the list 
               blockspace is the current space available to the current block
                          based on indentation
               after is distance from end of current block to next break
             *)
            fun printing ([], _, _)                    = () (* empty list returns the unit function *)
              | printing (e::es, blockspace, after)    = 
                (case e of 
                      (*
                         Go through the individual blocks recursively and printing them out
                         adjusting the blockspace and after. 
                      *) 
                      Block(bes, indent, len) => 
                        printing(bes, !space-indent, breakdist(es,after))
                      (* printing out a String is simple -- just print it out and reduce the 
                         available space by the length of the string *)
                    | String s  => (TextIO.output(os,s); space := !space - size s)
                      (* printing out a Break:
                            if the length of the break and the remaining expressions is less
                            than the remaining space -- just pad with blanks
                            otherwise, newline and then pad with margin - remaining space
                       *)
                    | Break len => 
                        if len + breakdist(es,after) <= !space
                        then blanks len
                        else (newLine(); blanks(margin-blockspace));
                    printing(es,blockspace,after))
        in printing([e],margin,0); newLine() end;
    end;
