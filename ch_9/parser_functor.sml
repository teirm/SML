(* functor for parser *)

use "../ch_9/lexical_sig.sml";
use "../ch_9/parse_sig.sml";

functor Parsing (Lex: LEXICAL) : PARSE =
    struct

    (* use the same token type as the Lex *)
    type token = Lex.token;

    exception SyntaxErr of string;
    
    (* Take an Id element off the token list and return the 
       string and remaining tokens.

       Raise a syntax exception if hd tokens is not an Lex.Id
    *)
    fun id (Lex.Id a :: tokens) = (a, tokens)
      | id tokens               = raise SyntaxErr "Identifier expected";

    (* See if head of tokens is a Lex.Key b and check if it equal
       to a. Raise a syntax error if not.

       Raise a syntax exception if hd tokens is not a Lex.Key
    *)
    fun $a (Lex.Key b :: tokens)   = if a=b then (a,tokens)
                                              else raise SyntaxErr a
      | $a _                       = raise SyntaxErr "Symbol expected";
   
    (* Return a pair of empty list and tokens *)
    fun empty tokens = ([], tokens);

    (* attempt to parse tokens with ph1 -- if that raises an exception
       try to parse with ph2

       Effective ph1 tokens orelse ph2 tokens
    *)
    fun (ph1 || ph2) tokens = ph1 tokens handle SyntaxErr _ => ph2 tokens;
   
    (* Fail if ph can't parse tokens *)
    fun !! ph tokens = ph tokens handle SyntaxErr msg => 
                                 raise Fail ("Syntax error: " ^ msg);
    
    (* 
       Consecutive parser -- do ph1 then ph2 and return the 
       parsed values and rmaining tokens
     *)
    fun (ph1 -- ph2) tokens =
        let val (x, tokens2) = ph1 tokens
            val (y, tokens3) = ph2 tokens2
        in ((x,y), tokens3) end;
    
    (* 
       Apply a function on the parsed token returning
       the result and remaining tokens
     *)
    fun (ph >> f) tokens = 
        let val (x, tokens2) = ph tokens
        in (f(x), tokens2) end;
    
    (* effectively do $a -- !!ph but only return the result of 
       ph -- so ignore the operation of $a.

       Stricter still -- fail if ph returns an error
    *)
    fun (a $-- ph) = ($a -- !!ph >> #2);

    (* Construct a list of meanings of a repeated phrase:

       Broken down by precedence:
         ph -- repeat ph returns a ((string * string list) * token list)
         this is modified by >> op:: to concatenate the string list to 
         give just 1 list.

         Eventually the repeated prhase might end and an exception raised
         this leads to the || which will return [] and any remaining tokens 
         at that point. This is the base case of the recursive calls.
    *)
    fun repeat ph tokens = (ph -- repeat ph >> (op::) || empty ) tokens;

    (* ... going to need to study this one *)
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
