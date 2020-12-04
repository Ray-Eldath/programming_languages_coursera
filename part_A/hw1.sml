fun is_older (d1: (int * int * int), d2: (int * int *int)) = 
let 
  val d1y = #1 d1
  val d1m = #2 d1
  val d1d = #3 d1
  val d2y = #1 d2
  val d2m = #2 d2
  val d2d = #3 d2
in
  d1y < d2y orelse 
  d1y = d2y andalso d1m < d2m orelse 
  d1y = d2y andalso d1m = d2m andalso d1d < d2d
end

fun number_in_month (dates: (int * int * int) list, month: int) = 
  if null dates then 0
  else let val ans_tl = number_in_month (tl dates, month)
  in 
    if (#2 (hd dates) = month) then 1 + ans_tl
    else ans_tl
  end

fun number_in_months (dates: (int * int * int) list, months: int list) =
  if null months then 0
  else number_in_month (dates, hd months) + number_in_months (dates, tl months)

fun dates_in_month (dates: (int * int * int) list, month: int) =
  if null dates then []
  else 
    let val ans_tl = dates_in_month (tl dates, month)
    in
      if (#2 (hd dates) = month) then hd dates :: ans_tl
      else ans_tl
    end

fun dates_in_months (dates: (int * int * int) list, months: int list) =
  if null months then []
  else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

fun get_nth (strings: string list, n: int) = 
  if n = 1 then hd strings
  else get_nth (tl strings, n - 1)

fun date_to_string (date: (int * int * int)) = 
let val months = ["January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December"]
in
  get_nth (months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
end

fun number_before_reaching_sum (s: int, l: int list) = 
let
  fun sum (ll: int list) = if null ll then 0 else hd ll + sum (tl ll)
  fun hd_n (ll: int list, n: int) = if n = 0 then [] else hd ll :: hd_n (tl ll, n - 1)
  fun searching (n: int) =
    if n > length l then 0
    else if (sum (hd_n (l, n)) < s andalso sum (hd_n (l, n + 1)) >= s) then n
    else searching (n + 1)
in
  searching 1
end

fun what_month (i: int) = 1 + number_before_reaching_sum (i, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])

fun month_range (d1: int, d2: int) =
  if d1 > d2 then []
  else what_month d1 :: month_range (d1 + 1, d2)

fun oldest (dates: (int * int * int) list) =
let
  fun oldest_dates (dl: (int * int * int) list) =
    if null (tl dl) then hd dl
    else let val ans_tl = oldest_dates (tl dl)
         in if is_older (hd dl, ans_tl) then hd dl
            else ans_tl
         end
in
  if null dates then NONE
  else SOME (oldest_dates dates)
end
