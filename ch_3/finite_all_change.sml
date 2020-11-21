(*
  finite_all_change.sml

  Compute all change possibilities
  only have a finite number of coints
*)

val gb_coins = [50,20,10,5,2,1]
and us_coins = [25,10,5,1];

fun finite_all_change  (coins, coinvals, count, 0)      = [coins]
  | finite_all_change  (coins, coinvals, 0,     amount) = []
  | finite_all_change  (coins, [],       count, amount) = []
  | finite_all_change  (coins, c::coinvals, count, amount) = 
        if amount<0 then []
        else finite_all_change(c::coins, c::coinvals, count-1, amount-c) @
             finite_all_change(coins, coinvals, count, amount);
