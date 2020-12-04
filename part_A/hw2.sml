(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)

fun same_string (s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (s: string, l: string list) =
  case l of
       [] => NONE
     | x::xs => if same_string (s, x)
                then SOME xs
                else case all_except_option (s, xs) of
                          NONE => NONE
                        | SOME ll => SOME (x::ll)

fun get_substitutions1 (sll: string list list, s: string) =
  case sll of
       [] => []
     | hd::tl => case all_except_option (s, hd) of
                      SOME r => r @ get_substitutions1 (tl, s)
                    | NONE => get_substitutions1 (tl, s)

fun get_substitutions2 (sll: string list list, s: string) =
let 
  fun getsub_helper (sll, s, acc) =
    case sll of
         [] => acc
       | hd::tl => case all_except_option (s, hd) of
                        SOME r => getsub_helper (tl, s, r @ acc)
                      | NONE => getsub_helper (tl, s, acc)
in
  getsub_helper (sll, s, [])
end

fun similar_names (sll, {first=f, middle=m, last=l}) =
let
  fun next_name firsts =
    case firsts of
         [] => []
       | hd::tl => {first=hd, middle=m, last=l} :: next_name tl
in
  next_name (f :: get_substitutions1 (sll, f))
end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (s, _) =
  case s of
       (Spades | Clubs) => Black
     | (Diamonds | Hearts) => Red

fun card_value (_, r) =
  case r of
       Num i => i
     | Ace => 11
     | _ => 10

fun remove_card (cs, c, exp) =
  case cs of
       [] => raise exp
     | hd::tl => if hd = c then tl
                 else hd :: remove_card (tl, c, exp)

fun all_same_color cs =
  case cs of
       [] => true
     | hd::[] => true
     | c1::c2::tl => card_color c1 = card_color c2 andalso 
                      all_same_color (c2 :: tl)

fun sum_cards cs =
let
  fun sum_helper (cs, acc) =
    case cs of
         [] => acc
       | hd::tl => sum_helper (tl, acc + card_value hd)
in
  sum_helper (cs, 0)
end

fun score (cs, goal) =
let 
  val sum = sum_cards cs
  val pscore = if sum > goal then 3 * (sum - goal)
               else goal - sum
in
  if all_same_color cs then pscore div 2
  else pscore
end

fun officiate (cs, ms, goal) =
let
  fun process (cs, ms, goal, hs) =
  let val s = score (hs, goal)
  in 
   if s < goal then
     case ms of
          [] => s
        | m::remains => case m of
                             Discard c => 
                             process (cs, remains, goal, 
                                      remove_card (hs, c, IllegalMove))
                           | Draw => case cs of
                                          [] => s
                                        | c::tl => process (tl, remains, goal, c :: hs)
   else s
  end
in
  process (cs, ms, goal, [])
end
