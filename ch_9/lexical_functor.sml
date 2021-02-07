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
    fun alphaNumTok str = if member(str, Keyword.alphas) then Key(str)
                            else case Int.fromString(str) of
                                    NONE            => Id(str)
                                  | SOME (integer)  => Num(integer);
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
    
    (* skip a comment range 

       Beginning with a substring, get the next character 'c' (NONE if end of substr).

       Check for symbols starting with 'c' and return the substring remaining 
       if the closeComment symbol is found.

       Nested comments are not supported.
     *)
    fun skipComment substr =
        (case Substring.getc substr of 
            NONE                => substr (* end of substring *)
          | SOME (c, substr1)   => let val (tok, substr1) = symbolic(String.str c, substr)
                                   in (case tok of
                                         Key (str)   => if str=Keyword.commentClose then substr1
                                                        else skipComment(substr1)
                                        | _          => skipComment(substr1)) 
                                   end);
   
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
                    in  case tok of
                            Key(str)    => if str=Keyword.commentOpen 
                                           then scanning(tokens, skipComment(substr2))
                                           else scanning(tok::tokens, substr2)
                          | _           => scanning (tok::tokens, substr2)
                    end
                else (* ignore spaces, line breaks, control characters *)
                    scanning (tokens, Substring.dropl (not o Char.isGraph) substr);
    
    fun scan a = scanning([], Substring.full a);
    end;
