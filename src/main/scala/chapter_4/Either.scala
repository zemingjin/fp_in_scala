package chapter_4

sealed trait Either[+E, +A] {
  // Exercise 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(b) => Left(b)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b()
      case Right(r) => Right(r)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this match {
      case Left(a) => Left(a)
      case Right(a) => Right(f(a, b))
    }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = Try(x / y)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
