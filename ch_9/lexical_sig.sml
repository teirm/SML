(* Signature for Lexer *)

signature LEXICAL = 
    sig 
    datatype token = Id of string 
                   | Key of string
                   | Num of int
    val scan : string -> token list
    end;
