fun pow (x: int, y: int) = 
  if y = 0
  then 1
  else x * pow(x, y - 1)

fun sum_list (xs: int list) = 
  if (null xs) then 0
  else hd xs + sum_list(tl xs)

fun countdown (x: int) = 
  if x = ~1 then []
  else x :: countdown (x - 1)

fun append (xs: int list, ys: int list) = 
  if null xs then ys
  else hd xs :: append (tl xs, ys)

fun sum_pair_list (xs: (int * int) list) = 
  if null xs then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list (tl xs)

fun firsts (xs: (int * int) list) = 
  if null xs then []
  else #1 (hd xs) :: firsts (tl xs)

fun seconds (xs: (int * int) list) = 
  if null xs then []
  else #2 (hd xs) :: seconds (tl xs)
