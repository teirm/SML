(* These are some basic list operations *)

(* Tests if a list is empty *)
fun null    []      = true
  | null    (_::_)  = false;

(* Partial function to get the head 
   of a list *)
fun hd (x::_) = x;

(* Partial function to return just 
   the tail *)
fun tl (_::xs) = xs; 

(* Partial function to return the 
   last element in a list *)
fun lst     [x]      = x
  | lst     (_::xs)  = lst(xs);

(* This is a slow recursive way to get
   the length of a list *)
fun nlength []       = 0
  | nlength (x::xs)  = 1 + nlength(xs);

(* Fast iterative way to get the length
   of a list. Note the use of an 
   accumulator *)
local
    fun addlen (n, [])  = n
      | addlen(n, x::l) = addlen(n+1, l)
in
    fun length l = addlen (0, l)
end;

fun upto (m,n) = 
    if m>n then [] else m::upto(m+1,n);

fun take ([], i)    = []
  | take (x::xs, i) = if i > 0 then x::take(xs, i-1) else [];

fun rtake ([], _, taken)    = taken
  | rtake (x::xs, i, taken) = 
        if i>0 then rtake(xs, i-1, x::taken) 
        else taken;

fun drop ([], _)    = []
  | drop (x::xs, i) = if i>0 then drop (xs, i-1)
                             else x::xs;

fun nth ([x], 0)    = x
  | nth (x::xs, i)  = nth(xs, i-1);

fun concat []       = []
  | concat (l::ls)   = l @ concat ls;

fun zip (x::xs, y::ys)  = (x,y)::zip(xs,ys)
  | zip _               = [];

local 
    fun conspair ((x,y),(xs,ys)) = (x::xs, y::ys);
in
    fun unzip []                 = ([], []) 
      | unzip (pair::pairs)      = conspair(pair, unzip pairs)
end;

fun my_zip ([], [])     = []
  | my_zip (x::xs, y::ys) = (x,y)::zip(xs,ys);

(* Remove the first occurence of an element from a list *)
fun remove_list_elem ([],p)         = []
  | remove_list_elem (x::xs,p)      = if p = x then xs
                                      else x::(remove_list_elem(xs,p));

fun remove_by_index ([], i)         = []
  | remove_by_index (x::xs, i)      = if i = 0 then xs
                                      else x::remove_by_index(xs, i-1);
