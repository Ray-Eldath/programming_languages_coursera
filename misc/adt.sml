abstype set = EMPTY | INS of int * set with 

val empty = EMPTY

fun contains(s, i) = 
  case s of 
       EMPTY => false
     | INS(n, r) =>
         if i = n then true 
         else contains(r, i)

exception SetOutOfBound

fun get(s, i) =
  case s of
       EMPTY => raise SetOutOfBound
     | INS(d, xs) =>
         if i = 0 then d
         else get(xs, i - 1)

fun insert(s, i) = 
  if not (contains(s, i)) 
  then INS(i, s) 
  else s
  
fun isEmpty(s) = (s = EMPTY)

fun union(s1, s2) = 
  case s1 of 
       EMPTY => s2
     | INS(n1, r1) => insert(union(r1, s2), n1) 

end
