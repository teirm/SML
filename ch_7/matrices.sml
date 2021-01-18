(* Generic Matrix Arithmetic *)

(* Only considering Zero Sum and Product *)
signature ZSP =
    sig
    type t
    val zero : t
    val sum  : t * t -> t
    val prod : t * t -> t
    end;

functor MatrixZSP (Z : ZSP) : ZSP = 
    struct
    type t      = Z.t list list;
    
    val  zero   = [];
    
    fun sum (rowsA, [])     = rowsA
      | sum ([], rowsB)     = rowsB
      | sum (rowsA, rowsB)  = ListPair.map (ListPair.map Z.sum)
                                           (rowsA, rowsB);

    fun dotprod pairs = foldl Z.sum Z.zero (ListPair.map Z.prod pairs);

    fun transp ([]::_)  = []
      | transp rows     = map hd rows :: transp(map tl rows);
    
    fun prod (rowsA, [])    = []
      | prod (rowsA, rowsB) = 
            let val colsB = transp rowsB
            in map (fn row => map (fn col => dotprod(row,col))
                                  colsB)
                   rowsA
            end;
    end;

structure IntZSP = 
    struct
    type t      = int;
    val zero    = 0;
    fun sum (x,y)   = x+y : t;
    fun prod (x,y)  = x*y : t;
    end;

(* Graph Applications -- Adjacency Matrix *)
structure BoolZSP = 
    struct
    type t      = bool;
    val zero    = false;
    fun sum (x,y) = x orelse y;
    fun prod (x,y) = x andalso y;
    end;

(* Path matrix to implement Floyd-Warshall Algorithm
   See algebra on a closed semi-ring *)
structure PathZSP = 
    struct 
    type t = int;
    val SOME zero   = Int.maxInt;
    val sum         = Int.min;
    fun prod(m,n)   = if m=zero orelse n=zero then zero
                      else m+n;
    end;

structure PathMatrix = MatrixZSP(PathZSP);

fun fast_paths mat = 
    let val n = length mat
        fun f (m,mat) = if n-1 <= m then mat
                        else f(2*m, PathMatrix.prod(mat,mat))
    in f(1,mat) end;
