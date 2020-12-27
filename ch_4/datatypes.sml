(*
    Some exercises on data types in SML
*)


datatype degree = Duke | Marquis | Earl | Viscount | Baron;
datatype person = King
                | Peer of string*string*int
                | Knight of string
                | Peasant of string
                | Esquire of string*string;

fun map_persons []                  = []
  | map_persons (King :: ps)        = 5 :: map_persons(ps)
  | map_persons (Peer(a,b,c)::ps)   = 4 :: map_persons(ps)
  | map_persons (Knight(s)::ps)     = 3 :: map_persons(ps)
  | map_persons (Peasant(s)::ps)    = 2 :: map_persons(ps)
  | map_persons (Esquire(a,b)::ps)  = 1 :: map_persons(ps)

fun superior_from_map(p1,p2) =
    let val [v1,v2] = map_persons([p1,p2])
    in if v1 > v2 then true
       else false
    end;

fun title King                  = "His Majesty the King"
  | title (Peer(deg, terr,_))   = "The " ^ deg ^ " of " ^ terr
  | title (Knight(name))        = "Sir " ^ name
  | title (Peasant(name))       = name
  | title (Esquire(name,vill))  = name ^ ", Esq., of " ^ vill;

type Point = real*real
datatype shapes = Circle of Point*real
                | Rectangle of Point*Point
                | Square of Point*real

fun distance ((x1,y1):Point, (x2,y2):Point) =
    Math.sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2));

fun area (Circle(_, r))                   = ((r*r)*3.14)
  | area (Square(_, s))                   = s*s
  | area (Rectangle((x1,y1), (x2,y2)))    =  Real.abs(x1-x2) * Real.abs(y1-y2);

(* merge implemented using case expression *)
fun merge(xlist, ylist) : real list = 
    case xlist of 
        []      => ylist
    |   x::xs   => (case ylist of 
                        []      => xlist
                    |   y::ys   => if x<=y then x::merge(xs, ylist)
                                           else y::merge(xlist, ys));

(* title implemented using case expression *)
fun case_title(person) =
    case person of 
        King                => "His Majesty the king"
    |   Peer(deg, terr, _)  => "The " ^ deg ^ " of " ^ terr
    |   Knight(name)        => "Sir " ^ name
    |   Peasant(name)       => name
    |   Esquire(name,vill)  => name ^ ", Esq., of " ^ vill;
    
