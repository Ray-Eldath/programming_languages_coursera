exception ListLengthUnmatched

fun zip3 t =
  case t of
       (t1::t1', t2::t2', t3::t3') => (t1, t2, t3) :: zip3 (t1', t2', t3')
     | (nil, nil, nil) => nil
     | _ => raise ListLengthUnmatched
     
fun unzip3 t =
  case t of
       (t1, t2, t3)::tl => let val (l1, l2, l3) = unzip3 tl
                           in 
                             (t1::l1, t2::l2, t3::l3)
                           end
     | nil => (nil, nil, nil)

fun nondecreasing xs =
  case xs of
       [] => true
     | _::[] => true
     | e1::e2::tl => e1 <= e2 andalso nondecreasing tl

datatype sgn = P | N | Z
fun multisign (x1, x2) =
let fun sign x = if x = 0 then Z else if x > 0 then P else N
in case (sign x1, sign x2) of
       (P, P) => P
     | (P, N) => N
     | (N, P) => N
     | (N, N) => P
     | _ => Z
end

fun len xs = 
  case xs of
       [] => 0
     | _::tl => 1 + len tl

fun fact n =
let fun fact_helper (x, acc) =
case x of
     0 => acc
   | _ => fact_helper (x - 1, acc * x)
in
  fact_helper (n, 1)
end
