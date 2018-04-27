package coursera.week4

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException
  override def tail: Nothing = throw new NoSuchElementException
}

object List {
  def apply[T](a: T, b: T): List[T] = new Cons(a, new Cons(b, Nil))
  def apply[T](): List[T] = Nil
}