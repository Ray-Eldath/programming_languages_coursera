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

fun only_capitals s =
  List.filter (fn x => Char.isUpper (String.sub (x, 0))) s

fun longest_string1 s =
  List.foldl (fn (x, max) => 
                if String.size x > String.size max
                then x else max) "" s

fun longest_string2 s =
  List.foldl (fn (x, max) => 
                if String.size x >= String.size max
                then x else max) "" s

fun longest_string_helper f s =
  List.foldl (fn (x, acc) =>
                if f (String.size x, String.size acc)
                then x else acc) "" s

val longest_string3 = 
  longest_string_helper (fn (a, b) => a > b)

val longest_string4 = 
  longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized = 
  longest_string1 o only_capitals

val rev_string =
  String.implode o List.rev o String.explode

fun first_answer f a =
  case a of
       [] => raise NoAnswer
     | x::xs =>
         case f x of
              SOME r => r
            | NONE => first_answer f xs

fun all_answers f xs =
let
  fun helper lst acc =
    case (lst, acc) of
         ([], _) => acc
       | (x::xs, SOME rs) => 
           (case f x of
                SOME r => helper xs (SOME (rs @ r))
              | NONE => NONE)
       | _ => NONE
in
  helper xs (SOME [])
end

val count_wildcards = 
  g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths =
  g (fn _ => 1) String.size

fun count_some_var (str, p) =
  g (fn _ => 0) (fn x => if x = str then 1 else 0) p

val check_pat =
let
  fun all_var pattern =
    case pattern of
         Variable x => [x]
       | TupleP ps => List.foldl (fn (p, acc) => acc @ all_var p) [] ps
       | ConstructorP (_, p) => all_var p
       | _ => []

  fun no_duplicate ss =
    case ss of
         [] => true
       | x::xs => not (List.exists (fn e => e = x) xs ) andalso no_duplicate xs
in
  no_duplicate o all_var
end

fun match (vp: valu * pattern) =
  case vp of
       (_, Wildcard) => SOME []
     | (v, Variable s) => SOME [(s, v)]
     | (Unit, UnitP) => SOME []
     | (Const c, ConstP i) => if c = i then SOME [] else NONE
     | (Tuple t, TupleP tp) => 
         (all_answers match (ListPair.zipEq (t, tp))
         handle UnequalLengths => NONE)
     | (Constructor (c, v), ConstructorP (cp, p)) =>
         if c = cp then match (v, p)
         else NONE
     | _ => NONE

fun first_match v ps =
let
  fun gen_matches x =
    case x of
         [] => []
       | hd::tl => (v, hd) :: gen_matches tl
in
  SOME (first_answer match (gen_matches ps))
  handle NoAnswer => NONE
end
