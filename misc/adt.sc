trait AdtList {
  type Rep

  val empty: Rep
  def isEmpty(rep: Rep): Boolean
  def insert[T](xs: Rep, x: T): Rep
  def contains[T](xs: Rep, x: T): Boolean
}

object AdtListImpl extends AdtList {
  sealed trait AdtListRep
  private case class Empty() extends AdtListRep
  private case class Inc[T](val data: T, val succ: AdtListRep) extends AdtListRep

  override type Rep = AdtListRep

  override val empty: AdtListRep = Empty()
  override def isEmpty(xs: AdtListRep) = xs == Empty()
  override def insert[T](xs: AdtListRep, x: T): AdtListRep = Inc(x, xs)
  override def contains[T](xs: AdtListRep, x: T) = xs match {
    case Empty() => false
    case Inc(c, xs) => if(c == x) true else contains(xs, x)
  }
}
