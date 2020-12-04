signature MYSET =
sig
  type ''a set
  val empty: ''a set
  val insert: ''a set * ''a -> ''a set 
  val contains: ''a set * ''a -> bool
end

structure AdtSet :> MYSET =
struct
  datatype 'a set = Empty | Cons of ('a * 'a set)

  val empty = Empty
  fun insert (s, v) = Cons (v, s)
  fun contains (s, v) =
    case s of
         Empty => false
       | Cons (c, xs) => if v = c then true
                         else contains (xs, v) 
end

structure ObjectsSet :> MYSET =
struct
  type 'a set = 'a -> bool

  val empty = fn _ => false
  fun insert (s, v) = fn i => i = v orelse s i
  fun contains (s, v) = s v
end
