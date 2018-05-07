package coursera.week5

class Week5 {
  def last[A](xs: List[A]): A = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
  }
}
