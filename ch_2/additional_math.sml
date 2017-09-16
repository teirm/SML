(* More math structures *)

signature ARITH =
 sig
 type t
 val zero : t
 val sum  : t * t -> t
 val diff : t * t -> t
 val prod : t * t -> t
 val quo  : t * t -> t
 end;


structure Real : ARITH = 
    struct
    type t          = real;
    val zero        = 0.0;
    fun sum (a, b)  = a + b : t;
    fun diff (a, b) = a - b : t;
    fun prod (a, b) = a * b : t;
    fun quo (a, b)  = a / b : t;
    end;

fun gcd (m,n) = if m = 0 then n else gcd(n mod m, m);

structure Rational : ARITH = 
    struct
    type t          = int * int;
    val zero        = (0, 1);
    fun sum ((x,y),(x',y'))  =
                                let val n = x*y' + y*x'
                                    val d = y*y'
                                    val f = gcd(n, d)
                                in ((n div f), (d div f)) end;
    fun diff ((x,y),(x',y')) =
                                let val n = x*y' - y*x'
                                    val d = y*y'
                                    val f = gcd(n, d)
                                in ((n div f), (d div f)) end;
    fun prod ((x,y),(x',y')) =
                                let val n = x*x'
                                    val d = y*y'
                                    val f = gcd(n,d)
                                in ((n div f), (d div f)) end;
    fun quo ((x,y),(x',y')) = 
                                let val n = x*y'
                                    val d = x'*y
                                    val f = gcd(n,d)
                                in ((n div f), (d div f)) end;
    end;
