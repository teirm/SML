(* A signature for sequences in SML *)

signature SEQUENCE =
  sig
  exception Empty
  type 'a seq
  (* provide access to constructors from signature *)
  val Nil           : 'a seq
  val Cons          : 'a * (unit -> 'a seq) -> 'a seq
  val cons          : 'a * 'a seq -> 'a seq
  val null          : 'a seq -> bool
  val hd            : 'a seq -> 'a
  val tl            : 'a seq -> 'a seq
  val fromList      : 'a list -> 'a seq
  val toList        : 'a seq  -> 'a list
  val take          : 'a seq * int -> 'a list
  val drop          : 'a seq * int -> 'a seq
  val @             : 'a seq * 'a seq -> 'a seq
  val interleave    : 'a seq * 'a seq -> 'a seq
  val map           : ('a -> 'b) -> 'a seq -> 'b seq
  val filter        : ('a -> bool) -> 'a seq -> 'a seq
  val iterates      : ('a -> 'a) -> 'a -> 'a seq
  val from          : int -> int seq
  end;
