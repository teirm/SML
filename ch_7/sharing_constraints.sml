(* sharing constraints between functor arguments *)

use "../ch_7/pq_sig.sml";
use "../ch_4/dictionary_sig.sml";

functor Join2 (structure PQueue : PRIORITY_QUEUE
               structure Dict   : DICTIONARY
               sharing type PQueue.Item.t = Dict.key) =
    struct
    fun lookmin(dict, pq) = Dict.lookup(dict, PQueue.min pq);
    end;
