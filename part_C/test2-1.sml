datatype Exp = Add of Exp * Exp | Int of int | String of string

fun add_values e1 e2 =
  case (e1, e2) of
       (Int i1, Int i2) => Int (i1 + i2)
     | (Int i1, String s2) => String (Int.toString i1 ^ s2)
     | (String s1, Int i2) => String (s1 ^ Int.toString i2)
     | (String s1, String s2) => String (s1 ^ s2)

fun eval x =
  case x of
       Int _ => x
     | String _ => x
     | Add (e1, e2) => add_values (eval e1) (eval e2)
