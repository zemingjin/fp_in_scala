package chapter_5

import Stream._

case object Empty extends Stream[Nothing] {
  override def toString: String = "Empty"
}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toString: String = "("+h()+","+t().toString+")"
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  /**
    * Exercise 5.8 - Generalize ones slightly to the function constant, which returns an infinite Stream of
    * a given value.
    */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /**
    * Exercise 5.9 - Write a function that generates an infinite stream of integers, starting from n, then n
    * + 1, n + 2, and so on.
    */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
    * Exercise 5.10 - Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1,
    * 2, 3, 5, 8, and so on.
    */
  val fibs: Stream[Int] = {
    def go(f_1: Int, f_2: Int): Stream[Int] =
      cons(f_1, go(f_2, f_1 + f_2))
    go(0, 1)
  }

  /**
    * Exercise 5.11 - Write a more general stream-building function called unfold. It takes an initial state,
    * and a function for producing both the next state and the next value in the generated
    * stream.
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  /**
    * Exercise 5.12 -Write fibs, from, constant, and ones in terms of unfold.
    */
  val fibsViaUnfold: Stream[Int] = unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Option((n, n + 1)))
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(a => Option((a, a)))
}

trait Stream[+A] {

  /*
   * Exercise 5.1
   * Write a function to convert a Stream to a List, which will force its evaluation and let you look at it in the
   * REPL. You can convert to the regular List type in the standard library. You can place this and other functions
   * that operate on a Stream inside the Stream trait.
   */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  /*
   * Exercise 5.2 - Write the function take(n) for returning the first n elements of a Stream, and drop(n) for
   * skipping the first n elements of a Stream.
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /*
   * Exercise 5.3 - Write the function takeWhile for returning all starting elements of a Stream that match
   * the given predicate.
   */
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  def exists(f: A => Boolean): Boolean = foldRight(false)((a, b) => f(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  /*
   * Exercise 5.4 - Implement forAll, which checks that all elements in the Stream match a given
   * predicate. Your implementation should terminate the traversal as soon as it encounters a
   * nonmatching value.
   */
  def forAll(f: A => Boolean): Boolean = foldRight(true)((a, b) => f(a) && b)

  /*
   * Exercise 5.5 - Use foldRight to implement takeWhile.
   */
  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else empty[A])

  /*
   * Exercise 5.6 - Implement headOption using foldRight.
   */
  def headOption: Option[A] = foldRight(None: Option[A])((h,_) => Some(h))

  /**
    * Exercise 5.7 - Implement map, filter, append, and flatMap using foldRight. The append method
    * should be non-strict in its argument.
    */
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))
  def filter(f: A => Boolean): Stream[A] = takeWhileViaFoldRight(f)
  def append[B >: A](that: => Stream[B]): Stream[B] = foldRight(that)((h, t) => cons(h, t))
  def flatMap[B >: A](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))

  /**
    * Exercise 5.13 - Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and
    * zipAll. The zipAll function should continue the traversal as long as either stream
    * has more elements—it uses Option to indicate whether each stream has been
    * exhausted.
    */
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this)({
                   case Cons(h, t) => Option((f(h()), t()))
                   case _ => None
                 })

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n))({
                        case (Cons(h, t), 1) => Some(h(), (empty, 0))
                        case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
                        case _ => None
                      })
  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this)({
                   case Cons(h, t) if f(h()) => Some(h(), t())
                   case _ => None
                 })
  def zipWithViaUnfold[B,C](that: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, that))({
                        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
                        case _ => None
               })
  def zipViaUnfold[B](s2: Stream[B]): Stream[(A,B)] = zipWithViaUnfold(s2)((_,_))
  def zipWithAllViaUnfold[B, C](that: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, that)) {
                                case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
                                case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
                                case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
                                case (Empty, Empty) => None
                              }
  def zipAllViaUnfold[B](that: Stream[B]): Stream[(Option[A], Option[B])] = zipWithAllViaUnfold(that)((_, _))

  /**
    * Exercise 5.14 -  Implement startsWith using functions you’ve written. It should check if one
    * Stream is a prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2)
    * would be true
    */
  def startsWithViaUnfold[A](that: Stream[A]): Boolean =
    zipAllViaUnfold(that).takeWhileViaUnfold(_._2.isDefined).forAll {
      case (h, h2) => h == h2
    }

}

