(* Purpose: Applications of lists
   Author:  Cyrus 
   Date:    17 September 2017 *)

fun change (coinvals, 0)                = []
  | change (c::coinvals, amount)        = 
        if amount < c then change(coinvals, amount)                 (* Get rid of largest coin val c *)
                      else c :: change(c::coinvals, amount -c );    (* Keep largest coin val c *)

(* Authors solution to 3.13 *)
fun allChange (coins, coinvals, 0, coinslist)      = coins::coinslist  (* Grow list as an additional param *) 
  | allChange (coins, [], amount, coinslist)       = coinslist 
  | allChange (coins, c::coinvals, amount, coinslist) = 
        if amount<0 then coinslist 
        else allChange(c::coins, c::coinvals, amount-c,
                       allChange(coins, coinvals, amount, coinslist));  (* Compute rest of list as 
                                                                           an argument *)
