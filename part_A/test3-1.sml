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

exception InvalidArgument

datatype poscounter = 
    poscounter of { value: int,
                    add:   int -> poscounter,
                    up:    unit -> poscounter,
                    minus: int -> poscounter,
                    down:  unit -> poscounter }

fun make_poscounter v = 
let
  fun up () = add 1
    and add i = make_poscounter (v + i)
  fun down () = minus 1
    and minus i = make_poscounter (v - i)
in
  if v < 0 then raise InvalidArgument
  else poscounter { value = v,
                    add   = add,
                    up    = up,
                    minus = minus,
                    down  = down }
end
