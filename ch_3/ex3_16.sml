(*
  Conversions between binary and decimal
*)

local  
    fun int_dec_to_bin (0, res) = res
      | int_dec_to_bin (x, res) = int_dec_to_bin(x div 2, (x mod 2) :: res);
in
    fun dec_to_bin(x) = int_dec_to_bin(x, []);
end;

fun bin_to_dec ([])    = 0 
  | bin_to_dec (x::xs) = x + 2*bin_to_dec(xs);
