(* Basic tautology checker in SML *)

(* needed for inter *)
use "../ch_3/set_ops.sml";

datatype prop = Atom of string
              | Neg  of prop
              | Conj of prop * prop
              | Disj of prop * prop;

(* p -> q is equivalent to (~p) v q
   Proof uses law of excluded middle: 
     ~p v p
*)
fun implies(p,q)  = Disj(Neg p,q);

(* convert proposition to a string *)
fun show (Atom a)       = a
  | show (Neg a)        = "(~" ^ show a ^ ")"
  | show (Conj(p,q))    = "(" ^ show p ^ " & " ^ show q ^ ")"
  | show (Disj(p,q))    = "(" ^ show p ^ " | " ^ show q ^ ")";

(* NNF - negation normal form:
   Push negations into conjunctions and disjunctions
     ~~p by p
     ~(p ^ q) by ~p v ~q
     ~(p v q) by ~p ^ ~q
*)
fun nnf (Atom a)            = Atom a
  | nnf (Neg (Atom a))      = Neg(Atom a)
  | nnf (Neg (Neg p))       = nnf p
  | nnf (Neg (Conj(p,q)))   = nnf(Disj(Neg p, Neg q))
  | nnf (Neg (Disj(p,q)))   = nnf(Conj(Neg p, Neg q))
  | nnf (Conj(p,q))         = Conj(nnf p, nnf q)
  | nnf (Disj(p,q))         = Disj(nnf p, nnf q);

(* Avoid needless construction of Negs with mutual
   recursion *)
fun nnfpos (Atom a)         = Atom a
  | nnfpos (Neg p)          = nnfneg p
  | nnfpos (Conj(p,q))      = Conj(nnfpos p, nnfpos q)
  | nnfpos (Disj(p,q))      = Disj(nnfpos p, nnfpos q)
and nnfneg (Atom a)         = Neg (Atom a)
  | nnfneg (Neg p)          = nnfpos p
  | nnfneg (Conj(p,q))      = Disj(nnfneg p, nnfneg q)
  | nnfneg (Disj(p,q))      = Conj(nnfneg p, nnfneg q);

(* CNF - conjunctive normal form:
   From NNF replace
     p v (q ^ r) by (p v q) ^ (p v r)
     (q ^ r) v p by (q v p) ^ (r v p)
*)
fun distrib(p, Conj(q,r))    = Conj(distrib(p,q), distrib(p,r))
  | distrib(Conj(q,r), p)    = Conj(distrib(q,p), distrib(r,p))
  | distrib(p,q)             = Disj(p,q) (*no conjunctions*);

fun cnf (Conj(p,q))         = Conj(cnf p, cnf q)
  | cnf (Disj(p,q))         = distrib(cnf p, cnf q)
  | cnf p                   = p (* a literal *);

exception NonCNF;
fun positives (Atom a)          = [a]
  | positives (Neg(Atom a))     = []
  | positives (Disj(p,q))       = positives p @ positives q
  | positives _                 = raise NonCNF;

fun negatives (Atom _)          = []
  | negatives (Neg(Atom a))     = [a]
  | negatives (Disj(p,q))       = negatives p @ negatives q
  | negatives _                 = raise NonCNF;

fun taut (Conj(p,q))            = taut p andalso taut q
  | taut p                      = not (null (inter (positives p, negatives p)));
