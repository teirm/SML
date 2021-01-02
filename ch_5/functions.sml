(* Functions as values in ML *)

val square = fn x:real => x*x;

val cons   = fn(x,y) => x::y;

val null   = fn []      => true
              | (_::_)  => false;

val area = fn r => 3.14 * r * r;
val title = fn name => "The duke of " ^ name;
val lengthvec = fn (x,y) => Math.sqrt(x*x + y*y);

(* poly morphic insertion sort *)
fun insort lessequal =
    let fun ins (x, [])     = [x]
          | ins (x, y::ys)  = 
                if lessequal(x,y) then x::y::ys
                                  else y::ins(x,ys)
        fun sort []         = []
          | sort (x::xs)    = ins (x, sort xs)
    in sort end;

(* Summation *)
fun summation f m =
    let fun sum (i,z) : real = 
            if i=m then z else sum(i+1, z+f(i))
    in sum(0, 0.0) end;

(* Polymorphic merge sort *)
fun tmergesort lessequal =
    let fun merge ([], ys)          = ys : real list
          | merge (xs, [])          = xs
          | merge (x::xs, y::ys)    = 
            if lessequal(x,y) then x::merge(xs,y::ys)
                              else y::merge(x::xs,ys);

        fun sort []     = []
          | sort [x]    = [x]
          | sort xs     = 
            let val k = length xs div 2
            in merge(sort(List.take(xs,k)),
                     sort(List.drop(xs,k)))
            end
    in sort end;

fun minimization f m = 
    let fun min (i,z) : real = 
            if i=m then z else 
                let val k = f(i)
                in min(i+1, if z < k then z else k)
                end
    in min(0,0.0) end;

fun double_min g m n = minimization (fn i => minimization (fn j => g(i,j)) n) m; 
