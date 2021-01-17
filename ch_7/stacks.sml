(* Creating some stacks as abstract types *)

(* Hold a stack as a list *)
abstype 'a stack1 = S1 of 'a list
    with 
    val empty = S1 [];

    fun push(S1 s, x) = S1(x::s);
    fun pop(S1(x::s)) = S1(s);
    fun peek(S1(x::s)) = x;
    fun snull(S1(x::s)) = false
      | snull _         = true;
    end;

(* Hold a stack as a recursive datatype *)
abstype 'a stack2 = Empty
                  | Stk of 'a * 'a stack2
    with 
    val empty = Empty
    and stk   = Stk;

    fun push(q, x) = Stk(x, q);

    fun pop(Stk(x, q))  = q;

    fun peek(Stk(x, q)) = x;

    fun snull(Stk _)    = false
      | snull Empty     = true;
    end;
