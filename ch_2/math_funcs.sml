(* Recursive practice with some taylor series *)


fun fact 0 = 1.0
  | fact n = (Real.fromInt n) * fact(n-1);

fun pow (x, 1) : real =  x
  | pow (x, n) : real=  x * pow (x,n-1);

(* Exponential *)
fun exp (_, 0) = 1.0
  | exp (x, n) = pow(x, n) / fact(n)  + exp(x, n-1);

(* Sin x *)
fun sin ((x, 0):(real*int)) = x 
  | sin (x, n) = if n mod 2 = 0 then (pow(x,2*n+1) / fact(2*n+1)) + sin(x, n-1)
                 else  ~(pow(x,2*n+1)/fact(2*n+1)) + sin(x,n-1);

(* Cos x *)
(* Mutual recursion *)
fun cos_even ((_, 0):(real*int)) = 1.0
  | cos_even ((x, n):(real*int)) = (pow(x,2*n)/fact(2*n)) + cos_odd(x,n-1)
and cos_odd  ((x, n):(real*int)) = ~(pow(x,2*n)/fact(2*n)) + cos_even(x,n-1);

fun cos((x,n):(real*int)) = if n mod 2 = 0 then cos_even(x,n) else cos_odd(x,n);
