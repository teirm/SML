(*
    Symbolic operations on Polynomials
*)

structure Poly = 
    struct
    type t                          = (int*real) list;
    val zero                        = []
    (* sum of two polynomials *)
    fun sum ([], us)                = us : t
      | sum (ts, [])                = ts
      | sum ((m,a)::ts, (n,b)::us)  = 
             if m>n then (m,a) :: sum (ts, (n,b)::us)
        else if n>m then (n,b) :: sum (us, (m,a)::ts)
        else (*m=n*)
            if Real.==(a,b)  then sum(ts,us)
                        else (m, a+b) :: sum (ts, us);
    fun termprod ((m,a), [])        = [] : t
      | termprod ((m,a), (n,b)::ts) = 
            (m+n, a*b)::termprod((m,a), ts);
    (* naieve multiplication *) 
    fun nprod ([], us)              = [] 
      | nprod ((m,a)::ts, us)       = sum (termprod((m,a), us),
                                           nprod (ts, us));
    (* divide and conquer multiplication *)
    fun prod ([], us)       = []
      | prod ([(m,a)], us)  = termprod((m,a), us)
      | prod (ts, us)       =
            let val k = (List.length ts) div 2
            in sum (prod(List.take(ts,k),us),
                    prod(List.drop(ts,k),us))
            end;

    (* print a polynomial *)
    fun show ([], s : string)   = s
      | show ((m,a)::xs, s)     = 
        let val m_str   = if m=0 then "" 
                          else if m=1 then "x" 
                          else "x^"^Int.toString(m)
            val a_str   = if Real.<(a,0.0) then Real.toString(~a)
                          else Real.toString(a)
            val sign    = if Real.<(a, 0.0) then "- " 
                          else " + " 
            val term    = sign ^ a_str ^ m_str
        in show(xs, s^term) 
        end;
    
    (*quotient and remainder *)
    fun quorem (ts, (n,b)::us)  = 
      let fun dviding ([],          qs) = (rev qs, [])
            | dividing ((m,a)::ts,  qs) = 
              if m<n then (rev qs, (m,a)::ts)
              else dividing (sum (ts, termprod ((m-n, ~a/b), us)),
                             (m-n, a/b)::qs)
      in dividing (ts, []) end;

    fun gcd ([], us) = us
      | gcd (ts, us)  = gcd(#2 (quorem(us,ts)), ts);
    end;


