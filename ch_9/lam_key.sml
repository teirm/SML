(* structures for parsing types and lambda calculus *)

use "../ch_9/lexical_functor.sml";
use "../ch_9/parser_functor.sml";

structure LamKey =
    struct val alphas       = ["list", "int", "string"]
           and symbols      = ["(", ")", "'", "->", ","]
           and commentOpen  = "(*"
           and commentClose = "*)"
    end;

structure LamLex     = Lexical(LamKey);
structure LamParsing = Parsing(LamLex);
