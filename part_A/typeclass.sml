signature COMPARABLE = sig
  type elem
  val compare: elem -> elem -> int
end

functor Ord (X: COMPARABLE) : sig
  val le: X.elem -> X.elem -> bool
  val eq: X.elem -> X.elem -> bool
  val ge: X.elem -> X.elem -> bool
end = struct
  fun le x y = X.compare x y >= 0
  fun eq x y = X.compare x y = 0
  fun ge x y = X.compare x y <= 0
end

structure IntComparable : COMPARABLE = struct
  type elem = int

  fun compare x y =
    case Int.compare (x, y) of
         LESS => ~1
       | EQUAL => 0
       | GREATER => 1
end
structure IntOrd = Ord(IntComparable)