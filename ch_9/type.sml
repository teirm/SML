(* structure of Type *)

use "../ch_8/pretty_printer.sml";
use "../ch_9/lam_key.sml";
use "../ch_9/type_sig.sml";

structure Type : TYPE = 
    struct

    datatype t = Con of string * t list | Var of string;

    local (** parsing **)
        fun makeFun (ty1,ty2) = Con("->", [ty1,ty2]);
        open LamParsing

        fun typ toks = 
            (   atom -- "->" $-- typ                >> makeFun
             || atom
            ) toks
        and atom toks = 
            (   $"'" -- id                          >> (Var o op^)
             || "(" $-- typ -- $")"                 >> #1
            ) toks;
        in
            val read = reader typ;
        end;

        local (** Display **)
            fun typ (Var a)                 = Pretty.str a
              | typ (Con("->",[ty1,ty2]))   = Pretty.blo(0, [atom ty1, 
                                                             Pretty.str " ->",
                                                             Pretty.brk 1,
                                                             typ ty2])
            and atom (Var a)                = Pretty.str a
              | atom ty                     = Pretty.blo(1, [Pretty.str "(",
                                                             typ ty,
                                                             Pretty.str ")"]);
        in 
            fun pr ty   = Pretty.pr (TextIO.stdOut, typ ty, 50)
        end
    end;
