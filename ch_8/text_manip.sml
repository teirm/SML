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
