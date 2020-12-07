signature OBJECTSLIST =
sig
  val isEmpty: bool
  val contains: ''a -> bool
  val insert: ''a -> OBJECTSLIST
end
