(* Lexical analysis functor *)

use "../ch_9/lexical_sig.sml";
use "../ch_9/keyword_sig.sml";

functor Lexical (Keyword : KEYWORD) : LEXICAL =
    struct

    datatype token = Key of string 
                   | Id  of string
                   | Num of int; 

    (* define a string specific member function *)

    fun member (x:string, l) = List.exists (fn y => x=y) l;
    
    (* Is the given string a keyword, number, or id 

        string -> token

     *)
    fun alphaNumToken str = if member(str, Keyword.alphas) then Key(str)
                          else case Int.fromString(str) of
                            NONE            => Id(str)
                        |   SOME (integer)  => Num(integer);
    
    (* scanning of a symbolic keyword 
       
       Given a current symbol string and a substring
       remove characters (ignoring punctuation) 
       from the substring until a symbol 
       is created returning the symbol
       and whatever remains of the substring.

       (string * substring) -> (string * substring)
     *)
    fun symbolic(symbol, substr) = 
        case Substring.getc substr of 
            NONE                    => (Key symbol, substr) (* end of substring *)
          | SOME (c,substr1)        => 
                if member(symbol, Keyword.symbols) orelse not (Char.isPunct c)
                then (Key symbol, substr)
                else symbolic(symbol ^ String.str c, substr1); 
   
    (* Scanning a substring into a list of tokens *)
    fun scanning (tokens, substr) =
        case Substring.getc substr of
            NONE                => rev tokens (* end of substring *)
         |  SOME (c, substr1)   => 
                if Char.isAlphaNum c then
                    let val (id, substr2) = Substring.splitl Char.isAlphaNum substr
                        val tok           = alphaNumTok(Substring.string id)
                    in scanning(tok::tokens, substr2)
                    end
                else if Char.isPunct c then (*  special symbol *)
                    let val (tok, substr2) = symbolic (String.str c, substr1) 
                    in scanning (tok::tokens, substr2)
                    end
                else (* ignore spaces, line breaks, control characters *)
                    scanning (tokens, Substring.dropl (not o Char.isGraph) substr);
    
    fun scan a = scanning([], Substring.all a);
    end;

(* Notes:
   For ex 9.2 -- just handle comments as special symbols. If one is 
   found drop characters in substr2 until the closing symbol is found
   pass the result into the recursive call to scanning
*)
