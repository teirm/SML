(* Functions as values in ML *)

val square = fn x:real => x*x;

val cons   = fn(x,y) => x::y;

val null   = fn []      => true
              | (_::_)  => false;

val area = fn r => 3.14 * r * r;
val title = fn name => "The duke of " ^ name;
val lengthvec = fn (x,y) => Math.sqrt(x*x + y*y);
