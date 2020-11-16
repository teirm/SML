(*
   Rational structure with ARITH signature
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

structure Rational : ARITH =
 struct
 type t     = int * int;
 val  zero  = (0, 1);
 fun  sum       ((n,d),(n',d'))   = let val den = d*d' in (n*d' + n'*d, den) end
 fun  diff      ((n,d),(n',d'))   = sum((n,d),(~n',d'));
 fun  prod      ((n,d),(n',d'))   = (n*n', d*d');
 fun  recip     (n,d)             = (d,n);
 fun  quo       ((n,d),(n',d'))   = prod((n,d), recip(n',d'));
 fun  gcd       (n,d)             = if n = 0 then 0 else gcd(d mod n, n);
 fun  check     (n,d)             = if d < 0 then (~n,~d) else (n,d);
 fun  simplify  (n,d)             = let val comm = gcd(n,d) in check(n div comm, d div comm) end
 end;
