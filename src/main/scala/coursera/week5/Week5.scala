package coursera.week5

case class Week5() {
  def last[T](a: List[T]): T = a match {
    case Nil => throw new Error("last of empty list")
    case List(h) => h
    case h :: t => last(t)
  }

  def init[T](a: List[T]): List[T] = a match {
    case List() => throw new Error("init of empty list")
    case List(h) => List()
    case h :: t => h :: init(t)
  }

  def concat[T](a: List[T], b: List[T]): List[T] = a match {
    case List() => b
    case h :: t => h :: concat(t, b)
  }

  def reverse[T](a: List[T]): List[T] = a match {
    case List() => a
    case h :: t => reverse(t) ++ List(h)
  }

  def removeAt[T](a: List[T], at: Int): List[T] = (a take at) ::: a.drop (at + 1)

  def flatten[T](a: List[Any]): List[Any] = a match {
    case List() => a
    case (h: List[Any]) :: t => flatten(h) ++ flatten(t)
    case h :: t => h :: flatten(t)
  }

  def msort[T](a: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = a.length / 2
    if (n == 0) a
    else {
      def merge(l1: List[T], l2: List[T]): List[T] = (l1, l2) match {
        case (Nil, _) => l2
        case (_, Nil) => l1
        case (x :: xs, y::ys) =>
          if (ord.lt(x, y)) x :: merge(xs, l2)
          else y :: merge(l1, ys)
      }

      val (fst, snd) = a splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  def scaleList(l: List[Double])(implicit factor: Double): List[Double] = l map (d => d * factor)
  def squareList(l: List[Int]): List[Int] = l map (d => d * d)
  def filter[T](l: List[T])(implicit p: T => Boolean): List[T] = l match {
    case Nil => Nil
    case h :: t => if (p(h)) h :: filter(t) else filter(t)
  }

  def posElems(l: List[Int])(p: Int => Boolean): List[Int] = filter(l)(p)

  def pack[T](l: List[T]): List[List[T]] = l match {
    case Nil => Nil
    case h :: _ =>
      val (first, rest) = l span (x => x == h)
      first :: pack(rest)
  }

  def encode[T](l: List[T]): List[(T, Int)] = pack(l) map(s => (s.head, s.length))

  def reduceLeft[T](l: List[T], op: (T, T) => T): T = l match {
    case Nil => throw new Error("Nil.reduceLeft")
    case h :: t => foldLeft(t, h)(op)
  }
  def foldLeft[U, T](l: List[T], z: U)(op: (U, T) => U): U = l match {
    case Nil => z
    case h :: t => (t foldLeft op(z, h))(op)
  }

  def reduceRight[T](l: List[T], op: (T, T) => T): T = l match {
    case Nil => throw new Error("Nil.reduceRight")
    case h :: Nil => h
    case h :: t => op(h, reduceRight(t, op))
  }
  def foldRight[U, T](l: List[T], z: U)(op: (T, U) => U): U = l match {
    case Nil => z
    case h :: t => op(h, foldRight(t, z)(op))
  }

  def sumViaReduceLeft(l: List[Int]): Int = reduceLeft(l, (a: Int, b: Int) => a + b)
  def sumViaFoldLeft(l: List[Int]): Int = foldLeft(l, 0)((a: Int, b: Int) => a + b)
  def sumViaReduceRight(l: List[Int]): Int = reduceRight(l, (a: Int, b: Int) => a + b)
  def sumViaFoldRight(l: List[Int]): Int = foldRight(l, 0)((a: Int, b: Int) => a + b)

  def concatViaFoldRight[T](l1: List[T], l2: List[T]): List[T] = foldRight(l1, l2)(_ :: _)

  def factorial(n: Int): Int =
    if (n == 0) 1
    else n * factorial(n - 1)
}
