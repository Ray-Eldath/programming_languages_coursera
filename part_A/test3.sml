fun fold f acc xs =
  case xs of
       [] => acc
     | x::tl => fold f (f (acc, x)) tl

fun map (f, xs) =
  case xs of
       [] => []
     | x::tl => (f x) :: (map (f, tl))

fun map_tr (f, xs) =
let
  fun helper (acc, f, xs) =
    case xs of
         [] => acc
       | x::tl => helper (acc @ [(f x)], f, tl)
(* seems no way to avoid the use of @ operator: here comes the tradeoff between
* tailrec and @, and the problem ensues: 
* under what circumstances is the tailrec truly worth it? *)

in 
  helper ([], f, xs)
end

fun map_f f = fold (fn (acc, x) => (f x)::acc) []

fun map_curried f = fn xs => map (f, xs)

fun filter (f, xs) =
  case xs of
       [] => []
     | x::tl => if f x then x::(filter (f, tl))
                else filter (f, tl)

fun filter_tr (f, xs) =
let
  fun helper (acc, f, xs) =
    case xs of
         [] => acc
       | x::tl => if f x then helper (acc @ [x], f, tl)
                  else helper (acc, f, tl)
in
  helper ([], f, xs)
end

fun filter_f f = 
  fold (fn (acc, x) => if f x then acc @ [x] else acc) []

fun all f =
  fold (fn (r, x) => r andalso f x) true

fun any f =
  fold (fn (r, x) => r orelse f x) false

infix 8 |>
fun x |> f = f x

infix 9 \>
fun f \> g = fn x => g (f x)


val callbacks: (int -> unit) list ref = ref []

fun register f = callbacks := f :: !callbacks

fun onEvent i =
let
  fun helper cbs =
    case cbs of
         [] => ()
       | f::fs => (f i; helper fs)
in helper (!callbacks)
end
