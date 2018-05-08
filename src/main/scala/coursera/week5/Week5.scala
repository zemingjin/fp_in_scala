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
}
