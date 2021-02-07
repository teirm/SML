(* signature for keywords *)
signature KEYWORD =
    sig
    val alphas  : string list
    and symbols : string list
    and commentOpen  : string
    and commentClose : string
    end;
