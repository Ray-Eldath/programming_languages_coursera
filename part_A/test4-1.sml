signature TST =
sig
  val valid: int -> bool
  val initial: int
end

structure Test1 :> TST =
struct
  fun valid x = x = 42
  val initial = 1
end

structure Test2 :> TST =
struct
  fun valid x = x = 213
  val initial = 20
  val pass = 40
end
