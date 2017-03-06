(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* No.1 *)
fun only_capitals xs = List.filter (fn x => Char.isUpper(String.sub(x,0))) xs

(* No.2 *)
fun longest_string1 xs =
  foldl(fn (x, y) => if String.size(x) > String.size(y) then x else y) "" xs       
(* No.3 *)
fun longest_string2 xs =
  foldl(fn (x, y) => if String.size(x) >= String.size(y) then x else y) "" xs
      
(* No.4 *)
fun longest_string_helper f =
  foldl(fn (x, y) => if f(String.size(x),String.size(y)) then x else y) ""

val longest_string3 = longest_string_helper(fn (x,y) =>  x > y)

val longest_string4 = longest_string_helper(fn (x,y) =>  x >= y)

(* No.5 *)
val longest_capitalized = longest_string1 o only_capitals

(* No.6 *)
val rev_string = String.implode o List.rev o String.explode

(* No.7 *)
fun first_answer f lst =
  case lst of
      [] => raise NoAnswer
    | hd::tl => case f hd of
		    SOME v => v
		  | NONE => first_answer f tl

(* No.8 *)
fun all_answers f lst =
  let fun aux (l, acc) =
	case l of
	    [] => SOME acc
	  | head::tail => case f head of
			      NONE => NONE
			    | SOME v => aux(tail, acc @ v)
  in
      aux(lst, [])
  end

(* No.9-a *)
val count_wildcards = g (fn () => 1) (fn str => 0)

(* No.9-b *)
val count_wild_and_variable_lengths = g (fn () => 1) String.size
			      
(* No.9-c *)
fun count_some_var (str, ptn) =
    g (fn () => 0) (fn s => if s = str then 1 else 0) ptn

(* No.10 *)
fun check_pat pat =
  let fun all_vars ptn =
	case ptn of
	    Variable x => [x]
	  | TupleP ps => List.foldl (fn (p,i) => i @ (all_vars p)) [] ps
	  | ConstructorP(_, p) => all_vars p
	  | _ => []

      fun is_uniq lst =
	case lst of
	    [] => true
	  | head::tail => (not (List.exists (fn str => str = head) tail))
			  andalso (is_uniq tail)
  in
      is_uniq(all_vars pat)
  end

      (* No. 11*)
fun match(va, pat) =
  case (va, pat) of
      (_, Wildcard) => SOME []
    | (v, Variable s) => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const i, ConstP j) => if i = j then SOME [] else NONE
    | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
			      then all_answers match (ListPair.zip(vs,ps))
			       else NONE
    | (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2
						   then match(v, p)
						   else NONE
    | (_, _) => NONE

(* No.12 *)
fun first_match va plst =
  SOME (first_answer (fn x => match(va, x)) plst) handle NoAnswer => NONE

