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

(* functionals *)

(* sections *)
fun secl x f y = f(x,y);
fun secr f x y = f(x,y);

(* combinators *)
(* composition *)
infix o;
fun (f o g) x = f (g x);

(* identity combinator *)
fun I x = x;

(* constant combinator *)
fun K x y = x;

(* general composition *)
fun S x y z = x z (y z);

(* filter *)
fun filter pred []      = []
  | filter pred (x::xs) = 
        if pred x then x :: filter pred xs
                  else      filter pred xs;

infix andf;
fun (p1 andf p2) x  = p1 x andalso p2 x;

fun takewhile pred []       = []
  | takewhile pred (x::xs)  = 
        if pred x then x :: takewhile pred xs
                  else [];

fun dropwhile pred []       = []
  | dropwhile pred (x::xs)  =
        if pred x then dropwhile pred xs
                  else x::xs;

fun exists pred   []        = false 
  | exists pred   (x::xs)   = (pred x) orelse (exists pred xs);

fun all pred   []        = true 
  | all pred   (x::xs)   = (pred x) andalso (all pred xs);

(* folds *)
fun foldl f e []        = e
  | foldl f e (x::xs)   = foldl f (f(x,e)) xs;

fun foldr f e []        = e
  | foldr f e (x::xs)   = f(x, foldr f e xs);
