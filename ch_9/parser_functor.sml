(* functor for parser *)

use "../ch_9/lexical_sig.sml";
use "../ch_9/parse_sig.sml";

functor Parsing (Lex: LEXICAL) : PARSE =
    struct

    (* use the same token type as the Lex *)
    type token = Lex.token;

    exception SyntaxErr of string;

    fun id (Lex.Id a :: tokens) = (a, tokens)
      | id tokens               = raise SyntaxErr "Identifier expected";

    fun $a (Lex.Key b :: tokens)   = if a=b then (a,tokens)
                                              else raise SyntaxErr a
      | $a _                       = raise SyntaxErr "Symbol expected";
    
    fun empty tokens = ([], tokens);

    fun (ph1 || ph2) tokens = ph1 tokens handle SyntaxErr _ => ph2 tokens;
    
    fun !! ph tokens = ph tokens handle SyntaxErr msg => 
                                 raise Fail ("Syntax error: " ^ msg);
    
    fun (ph1 -- ph2) tokens =
        let val (x, tokens2) = ph1 tokens
            val (y, tokens3) = ph2 tokens2
        in ((x,y), tokens3) end;

    fun (ph >> f) tokens = 
        let val (x, tokens2) = ph tokens
        in (f(x), tokens2) end;

    fun (a $-- ph) = ($a -- !!ph >> #2);

    fun repeat ph tokens = (ph -- repeat ph >> (op::) || empty ) tokens;

    fun infixes (ph, prec_of, apply) = 
        let fun over k tokens = next k (ph tokens)
            and next k (x, Lex.Key(a)::tokens) = 
                if prec_of a < k then (x, Lex.Key a :: tokens)
                else next k ((over (prec_of a) >> apply a x) tokens)
              | next k (x, tokens) = (x, tokens)
        in over 0 end;

    (* scan and parse, checking no tokens remain *)
    fun reader ph a = 
        (case ph (Lex.scan a) of 
              (x, [])   => x
            | (_, _::_) => raise SyntaxErr "Extra characters in phrase");
    end;
