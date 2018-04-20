package chapter_4

import scala.{Either => _, None => _, Option => _, Some => _}

sealed trait Option[+A] {
  case object None extends Option[Nothing]
  case class Some[+A](get: A) extends Option[A]

  // Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a) else None
  }

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String) : Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    None
  }
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  // Exercise 4.3
//  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aa => b.map(bb => f(aa, bb)))

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h::t => h.flatMap(hh => sequence(t).map(hh :: _))
  }
  // Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f))(_::_)
  }
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
}



