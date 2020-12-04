fun match xs =
let
  fun match_one xs =
    case xs of
         [] => true
       | 1::xs => match_two xs
       | _ => false
  and match_two xs =
      case xs of
           [] => true
         | 2::xs => match_one xs
         | _ => false
in
  match_one xs
end
