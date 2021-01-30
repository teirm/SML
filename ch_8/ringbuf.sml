(* Ring Buffer Implementation in SML *)

use "../ch_8/ringbuf_sig.sml";

structure RingBuf :> RINGBUF =
    struct
    datatype 'a buf = Nil | Node of 'a buf ref * 'a * 'a buf ref;
    datatype 'a t   = Ptr of 'a buf ref;
    exception Empty;

    fun left (Node(lp,_,_)) = lp
      | left Nil            = raise Empty;

    fun right (Node(_,_,rp)) = rp
      | right Nil            = raise Empty;

    fun empty() = Ptr(ref Nil);

    fun null (Ptr p) = case !p of
                Nil         => true
              | Node(_,x,_) => false;

    fun label (Ptr p) = case !p of 
                Nil         => raise Empty
              | Node(_,x,_) => x;

    fun moveLeft (Ptr p)    = (p := !(left(!p)));
    fun moveRight (Ptr p)   = (p := !(right(!p)));

    fun insertLeft (Ptr p, x)   = 
        case !p of 
            Nil         =>
                let val lp  = ref Nil
                    and rp  = ref Nil
                    val new = Node(lp,x,rp)
                in lp := new; rp := new; p := new end
        |   Node(lp,_,_) =>
                let val new = Node(ref(!lp), x, ref(!p))
                in right(!lp) := new; lp := new end;
   
    fun insertRight (Ptr p, x) =
        case !p of 
            Nil         => 
                let val lp  = ref Nil
                    and rp  = ref Nil
                    val new = Node(lp, x, rp)
                in lp := new; rp := new; p := new end
        |   Node(_,_,rp) =>
                let val new = Node(ref(!p), x, ref(!rp))
                in left(!rp) := new; rp := new end;

    fun delete (Ptr p) =
        case !p of 
            Nil             => false 
          | Node(lp,x,rp)   =>
                (if left(!p) = lp then p := Nil
                 else (right(!lp) := !rp; left(!rp) := !lp; p := !rp);
                 true)
    end;
