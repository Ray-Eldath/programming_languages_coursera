sealed trait AdtList

private case class Empty() extends AdtList
private case class Inc[T](val data: T, val succ: AdtList) extends AdtList

def empty(): AdtList = Empty()
def insert[T](xs: AdtList, x: T): AdtList = Inc(x, xs)
def isEmpty(xs: AdtList) = xs == Empty
def contains[T](xs: AdtList, x: T): Boolean = xs match {
  case Empty() => false
  case Inc(c, xs) => if(c == x) true else contains(xs, x)
}
