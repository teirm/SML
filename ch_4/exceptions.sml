(*
    Exceptions and Handling in SML
*)

exception Change;
fun backChange (coinvals, 0)            = []
  | backChange ([], amount)             = raise Change
  | backChange (c::coinvals, amount)    =   
    if amount<0 then raise Change
    else c :: backChange(c::coinvals, amount-c)
         handle Change => backChange(coinvals, amount);

