(* A signature for trees *)

signature TREE = 
    sig
    datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree
    val size        : 'a tree -> int
    val depth       : 'a tree -> int
    val reflect     : 'a tree -> 'a tree
    val preord      : 'a tree -> 'a list
    val inorder     : 'a tree -> 'a list
    val postorder   : 'a tree -> 'a list
    val balpre      : 'a list -> 'a tree
    val balin       : 'a list -> 'a tree
    end;
