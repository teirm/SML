(* A program that converts a gregorian date
   into roman numerals *)

val roman_numerals = [("M", 1000), ("D", 500), 
                      ("C", 100), ("L", 50),
                      ("X", 10), ("V", 5), ("I", 1)];

fun date_converter (roman_numerals, 0)      = []
  | date_converter ((l, v)::roman_numerals, date) = 
        if date < v then date_converter(roman_numerals, date)
        else l::date_converter((l,v)::roman_numerals, date-v);
