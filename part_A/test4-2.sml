signature MYRATIONAL =
sig
  type Rational
  val Whole: int -> Rational

  exception InvalidDominator
  val make_frac: int * int -> Rational
  val to_string: Rational -> string
end

structure MyRational :> MYRATIONAL =
struct
  datatype Rational = Whole of int | Frac of int * int
  
  exception InvalidDominator

  fun gcd (x, y) =
    if x = y then x
    else if x < y then gcd (x, y - x)
    else gcd (y, x)

  fun reduce d =
    case d of
         Whole _ => d
       | Frac (_, 0) => Whole 0
       | Frac (0, _) => Whole 0
       | Frac (x, 1) => Whole x
       | Frac (x, y) =>
           let val g = gcd (abs x, y)
           in if g = y then Whole (x div g)
              else Frac (x div g, y div g)
           end

  fun make_frac (x, y) =
    if y = 0 then raise InvalidDominator
    else 
      if x < 0 then reduce (Frac (~x, ~y))
      else reduce (Frac (x, y))

  fun to_string x =
    case x of
         Whole i => Int.toString i
       | Frac (a, b) => Int.toString a ^ "/" ^ Int.toString b
end
