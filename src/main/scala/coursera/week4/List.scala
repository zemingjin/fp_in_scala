package coursera.week4

case class Cons[T](head: T, tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException
  override def tail: Nothing = throw new NoSuchElementException
}

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)

  def last(): T = this match {
    case Nil => throw new Error("last of empty list")
    case Cons(h, Nil) => h
    case Cons(_, t) => t.last()
  }
}

object List {
  def apply[T](a: T, b: T): List[T] = new Cons(a, new Cons(b, Nil))
  def apply[T](): List[T] = Nil
}