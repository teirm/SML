(*
    Binary arithmetic in SML
*)

structure Bin =
    struct 
    fun carry (0, ps)    = ps    (* no carry *)
      | carry (1, [])    = [1]   (* carry off MSB *)
      | carry (1, p::ps) = (1-p)::carry(p, ps);

    fun sum (c, [], qs)          = carry(c, qs)
      | sum (c, ps, [])          = carry(c, ps)
      | sum (c, p::ps, q::qs)    = 
          ((c+p+q) mod 2)::sum((c+p+q) div 2, ps, qs);

    fun prod ([], _)             = []                 (* a * 0 = 0 *)
      | prod (0::ps, qs)         = 0::prod(ps,qs)  (* zero add -- so just shift *)
      | prod (1::ps, qs)         = sum(0, qs, 0::prod(ps,qs)); (* qs get added and then shift rest *)
    end;
