(*
   Product and sum of boolean list
*)

local 
   fun sum([], res)    =  res 
     | sum(x::xs, res) = sum(xs,x orelse res);
in fun bool_sum(xs) = sum(xs, false)
end;

local
   fun product([], res)     = res
     | product(x::xs, res)  = product(xs, x andalso res);
in fun bool_product(xs) = product(xs, true)
end;
