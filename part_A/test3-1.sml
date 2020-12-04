datatype 'a set = set of { insert: 'a -> 'a set, 
                           contains: 'a -> bool,
                           size: unit -> int }

fun make_set xs =
let 
  fun contains i = List.exists (fn e => e = i) xs
in
  set { insert = fn i => if contains i
                         then make_set xs
                         else make_set (i::xs),
        contains = contains,
        size = fn () => List.length xs }
end

fun empty_set () = make_set []


datatype counter = counter of { value: int,
                                add: int -> counter,
                                up: unit -> counter,
                                minus: int -> counter,
                                down: unit -> counter }

fun make_counter v = 
let
  fun add i = make_counter (v + i)
  fun minus i = make_counter (v - i)
in
  counter { value = v,
            add = add,
            up = fn () => add 1,
            minus = minus,
            down = fn () => minus 1 }
end
