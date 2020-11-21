(*
   all_change without append operation

   Note: This is very much like a foldl!
   Look and see where the accumulator is
   and how it is all folding down across the function
   calls:
    f(a,b,f(c,d,[])) -> f(a,b,[c,d]) -> [a,b,c,d]
*)

fun allChange (coins, coinvals, 0, accum)   = coins::accum
  | allChange (coins, [],       amount, accum) = accum 
  | allChange (coins, c::coinvals, amount, accum) =
    if amount<0 then accum  
    else allChange(c::coins, c::coinvals, amount-c, allChange(coins,coinvals,amount,accum));
