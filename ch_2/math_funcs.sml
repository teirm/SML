(* Recursive practice with some taylor series *)


fun fact 0 = 1.0
  | fact n = (Real.fromInt n) * fact(n-1);

fun pow (x, 1) = Real.fromInt x
  | pow (x, n) = Real.fromInt x * pow (x,n-1);

(* Exponential *)
fun exp (_, 0) = 1.0
  | exp (x, n) = pow(x, n) / fact(n)  + exp(x, n-1);

(* Sin x *)
fun sin (x, 0) = x
  | sin (x, n) = if n mod 2 = 0 then pow(x,n+2) / fact(2*n+1) + sin(x, n-1)
                 else ~(pow(x,n+1)/fact(2*n+1)) + sin(x,n-1); 
