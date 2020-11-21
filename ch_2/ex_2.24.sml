(* 
exercise 2.24
Structure real with signature ARITH
*)

signature ARITH = 
  sig
  type t
  val zero  : t
  val sum   : t * t -> t
  val diff  : t * t -> t
  val prod  : t * t -> t
  val quo   : t * t -> t
  end;


structure Real : ARITH =
  struct 
  type t            = real;
  val  zero         = 0.0;
  fun  sum      (x:t, y:t) = x+y : t;
  fun  diff     (x:t, y:t) = sum(x,~y) : t;
  fun  prod     (x:t, y:t) = x*y : t;
  fun  quo      (x:t, y:t) = x/y : t;
  end;
