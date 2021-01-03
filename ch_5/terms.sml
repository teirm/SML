(* Operating on terms *)

use "functions.sml";

datatype term = Var of string
              | Fun of string * term list;

fun subst f (Var a)             = f a
  | subst f (Fun(name,args))    = Fun(name, map(subst f) args);

fun vars (Var a)                = [a]
  | vars (Fun(_,args))          = List.concat(map vars args);

fun funs (Fun(a,args))          = List.concat([a]::(map funs args))
  | funs (Var a)                = [];

fun accumVars (Var a,bs)         = a::bs
  | accumVars (Fun(_,args),bs)   = foldr accumVars bs args;

fun accumFuns (Fun(a,args),bs)      = a::(foldr accumFuns bs args)
  | accumFuns (Var a, bs)           = bs;

fun replace t a b = if a=b then t else Var b;


