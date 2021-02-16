signature TST =
sig
  val valid: int -> bool
  val initial: int
end

functor TSTOne (X: TST) :
sig
    val isOneValid: bool
end = struct
    val isOneValid = X.valid 1
end

structure Test1 :> TST =
struct
  fun valid x = x = 42
  val initial = 1
end

structure Test1One = TSTOne(Test1)

structure Test2 :> TST =
struct
  fun valid x = x = 213
  val initial = 20
  val pass = 40
end