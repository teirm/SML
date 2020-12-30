(*
    binary tree library
*)

datatype 'a tree = Lf
                 | Br of 'a * 'a tree * 'a tree;

datatype 'a list = End
                 | Node of 'a * 'a list;

datatype ('a,'b)ltree = LLf of 'b
                      | LBr of 'a * ('a,'b)ltree * ('a,'b)ltree;

datatype 'a mtree = MLf
                  | MBr of 'a * 'a mtree list;

(* return the number of labels in a tree *)
fun size Lf                 = 0
  | size (Br(v,t1,t2))      = 1 + size(t1) + size(t2);

(* depth of the tree *)
fun depth Lf                = 0
  | depth (Br(v,t1,t2))     = 1 + Int.max(depth(t1), depth(t2));

(* construct a balanced tree of labels 1 to 2^n *)
fun comptree(k,n)  = 
    if n = 0 then Lf
             else Br(k, comptree(2*k, n-1),
                        comptree(2*k+1, n-1));

(* construct a balanced tree of depth n containing only x *)
fun compsame(x,n) = 
    if n = 0 then Lf
    else let val t = compsame(x, n-1) in
             Br(x, t, t) end;

(* reflect a tree by constructing a new tree *)
fun reflect Lf            = Lf
  | reflect (Br(v,t1,t2)) = Br(v, reflect(t2), reflect(t1));

(* determine if a binary tree is balanced based on
   balanced = | sizeof(t1) - sizeof(t2)| <= 1
*)
fun balanced Lf             = true
  | balanced(Br(v,t1,t2))   = if abs(size(t1)-size(t2)) <= 1 then 
                                 if balanced(t1) then balanced(t2)
                                 else false
                              else false;

(* Check if two trees satisfy
     t = reflect(u)
*)
fun check_reflect(Lf, Lf)   = true
  | check_reflect(Br(v1,t1,t2),Br(v2,s1,s2)) = 
        if v1=v2 then check_reflect(t1,s2) andalso check_reflect(t2,s1)
        else false
  | check_reflect(_,_) = false;

(* tree traversals *)
fun preord (Lf, vs)             = vs
  | preord (Br(v,t1,t2), vs)    = v :: preord(t1, preord(t2,vs));

fun inord (Lf, vs)             = vs
  | inord (Br(v,t1,t2), vs)    = inord(t1, v::inord(t2,vs));

fun postord (Lf, vs)             = vs
  | postord (Br(v,t1,t2), vs)    = postord(t1, v::postord(t2,vs));

(* Building a balanced tree form a preorder list *)
fun balpre []       = Lf
  | balpre(x::xs)   = 
    let val k = length xs div 2
    in Br(x, balpre(List.take(xs,k)), balpre(List.drop(xs,k)))
    end;

(* Building a balanced tree from inorder list *)
fun balin []        = Lf
  | balin xs        = 
    let val k       = length xs div 2
        val y::ys   = List.drop(xs,k)
    in Br(y, balin(List.take(xs,k)), balin ys)
    end;

(* Building a balanced tree from post order list *)
fun balpost xs      = reflect(balpre(List.rev xs));  
