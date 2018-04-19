package chapter_3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
   }

  def product(ds: List[Double]): Double =
    ds match {
     case Nil => 1.0
     case Cons(0.0, _) => 0.0
     case Cons(x, xs) => x * product(xs)
   }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs
    }

  def setHead[A](a: A, as: List[A]): List[A] =
    as match {
      case Nil => Cons(a, Nil)
      case Cons(_, t) => Cons(a, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
      case _ => l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)
  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  // Exercise 3.9
  def length[A](as: List[A]): Int = as match {
    case Nil => 0
    case Cons(_, t) => 1 + length(t)
  }

  // Exercise 3.10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((a, _) => a + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  // Exercise 3.13
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = foldLeft(reverse(l), z)((b, a) => f(a, b))
  def sum4(l: List[Int]): Int = foldRightViaFoldLeft(l, 0)(_ + _)
  def product4(l: List[Double]): Double = foldRightViaFoldLeft(l, 1.0)(_ * _)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, z)((a, b) => f(b, a))
  def sum5(l: List[Int]): Int = foldLeftViaFoldRight(l, 0)(_ + _)
  def product5(l: List[Double]): Double = foldLeftViaFoldRight(l, 1.0)(_ * _)
  def length3[A](l: List[A]): Int = foldLeftViaFoldRight(l, 0)((a, _) => a + 1)

  // Exercise 3.14
  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = foldLeft(reverse(l), r)((z, h) => Cons(h, z))
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))

  // Exercise 3.15
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(appendViaFoldRight)

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  // Exercise 3.16
  def add(l: List[Int]): List[Int] = foldRight(l, List[Int]())((a, acc) => Cons(a + 1, acc))

  // Exercise 3.17
  def toString(l: List[Double]): List[String] = foldRight(l, List[String]())((a, acc) => Cons(a.toString, acc))

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((a, acc) => Cons(f(a), acc))
  def toStringViaMap(l: List[Double]): List[String] = map(l)(a => a.toString)

  // Exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, List[A]())((a, acc) => if (f(a)) Cons(a, acc) else acc)
  // Exercise 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))
  // Exercise 3.21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(h => if (f(h)) List(h) else Nil)
  // Exercise 3.22
  def addLists(a: List[Int], b: List[Int]): List[Int] = zipWith(a, b)((a, b) => a + b)
  // Exercise 3.23
  def zipWith[A, B](a: List[A], b: List[A])(f: (A, A) => B): List[B] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
  // Exercise 3.24
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => false
    case Cons(h, t) =>
      if (startsWith(l, sub)) true
      else hasSubsequence(t, sub)
  }

}
