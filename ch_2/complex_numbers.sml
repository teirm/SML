(* Learning Structures in SML *)

structure Complex = 
    struct
    type t      = real * real;
    val zero    = (0.0, 0.0);
    fun sum     ((x,y), (x',y')) = (x+x', y+y') : t;
    fun diff    ((x,y), (x',y')) = (x-x', y-y') : t;
    fun prod    ((x,y), (x',y')) = (x*x'- y*y', x*y' + x'*y) : t;
    fun recip   (x,y)            = 
                                   let val t = x*x + y*y;
                                   in (x/t, ~y/t) end;
    fun quo     (z,z')           = prod(z, recip z')
    end;

