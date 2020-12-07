signature ADTLIST =
sig
  type 'a AdtList

  val empty: ''a AdtList
  val insert: ''a AdtList -> ''a -> ''a AdtList
  val isEmpty: ''a AdtList -> bool
  val contains: ''a AdtList -> ''a -> bool
end

structure AdtList :> ADTLIST =
struct
  datatype ''a AdtList = EMPTY | CONS of ''a * ''a AdtList

  val empty = EMPTY
  fun insert xs x = CONS (x, xs)
  fun isEmpty xs = xs = EMPTY
  fun contains xs x =
    case xs of
         EMPTY => false
       | CONS (c, xs) => if c = x then true
                  else contains xs x
end
