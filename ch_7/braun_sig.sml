(* signature for braun trees *)

use "../ch_7/tree_sig.sml";

signature BRAUN = 
    sig
    structure Tree: TREE
    val sub     : 'a Tree.tree * int -> 'a
    val update  : 'a Tree.tree * int * 'a -> 'a Tree.tree
    val delete  : 'a Tree.tree * int -> 'a Tree.tree
    val loext   : 'a Tree.tree * 'a -> 'a Tree.tree;
    val lorem   : 'a Tree.tree -> 'a Tree.tree;
    end;
