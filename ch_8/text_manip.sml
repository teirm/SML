(* text manipulation and IO in SML *)

fun writeCheck w (dols, cents) =
    let val dols_str    = Int.toString dols
        val cents_str   = StringCvt.padLeft #"0" 2 (Int.toString cents)
        val money_str   = dols_str ^ "." ^ cents_str
    in "$" ^ StringCvt.padLeft #"*" w money_str end;

fun stringMap f input = 
    let val chars = explode input 
        fun stringMapInt ([], s)      = s
          | stringMapInt ((x::xs), s) = stringMapInt(xs, s ^ (Char.toCString (f(x))))
    in stringMapInt (chars, "") end;

fun toUpper input = stringMap Char.toUpper input; 
fun toLower input = stringMap Char.toLower input;

(* Get the first character of a string s *)
fun firstChar s = String.sub(s,0);

(* Produce a string of the first character of every word in input *)
val initials = implode o (map firstChar) o (String.tokens Char.isSpace);

fun batchInitials (is, os) = 
    while not (TextIO.endOfStream is)
    do TextIO.output(os, initials (Option.getOpt((TextIO.inputLine is), "\n") ^ "\n"));

(* There is something slightly strange with how PolyML handles this *)
fun promptInitials (is, os) = 
    while (TextIO.output(os, "Input Line? ");
           TextIO.flushOut(os);
           not (TextIO.endOfStream is))
    do TextIO.output(os, "Initials:     " ^
                          initials(valOf (TextIO.inputLine is)) ^ "\n");

(* HTML converter *)
fun firstLine s = 
    let val (name, rest) = Substring.splitl (fn c => c <> #".") (Substring.full s)
    in "\n<P><EM>"  ^ Substring.string name ^
       "</EM>"      ^ Substring.string rest
    end;

fun htmlCvt fileName = 
    let val is = TextIO.openIn  fileName
        and os = TextIO.openOut (fileName ^ ".html")
        fun cvt _ ""    = ()
          | cvt _ "\n" = cvt true (Option.getOpt(TextIO.inputLine is, ""))
          | cvt first s = 
                    (TextIO.output (os,
                                    if first then firstLine s
                                    else "<BR>" ^ s);
                     cvt false (Option.getOpt(TextIO.inputLine is, "")));
    in cvt true "\n"; TextIO.closeIn is; TextIO.closeOut os
    end;

(* Count Lines in a file *)
fun charCount line = length (explode line);
fun wordCount line = length (String.tokens Char.isSpace line);
fun lineCount fileName = 
    let val inStream = TextIO.openIn fileName
        and char_count = ref 0
        and word_count = ref 0
        and line_count = ref 0
    in while(not (TextIO.endOfStream inStream))
       do (
               let val line = valOf (TextIO.inputLine inStream)
               in char_count := !char_count + charCount line;
                  word_count := !word_count + wordCount line
               end;
               line_count := !line_count + 1
          );
       (!line_count,!word_count,!char_count)
    end;

