trait ObjectsList[+T] {
  val isEmpty: Boolean
  def contains[X >: T](value: X): Boolean
  def insert[X >: T](value: X): ObjectsList[X]
}

case class Empty[T]() extends ObjectsList[T] {
  override val isEmpty = true
  override def insert[X >: T](value: X) = Insert(value, this)
  override def contains[X >: T](value: X) = false
}

case class Insert[T, S <: ObjectsList[T]](data: T, xs: S) extends ObjectsList[T] {
  override val isEmpty = false
  override def insert[X >: T](value: X) = Insert(value, this)
  override def contains[X >: T](value: X) = data == value || xs.contains(value)
}
