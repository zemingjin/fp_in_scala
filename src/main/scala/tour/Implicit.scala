package tour

abstract class Monoid[A] {
  def add(x: A, y: A): A
  def unit: A
}

class Implicit {
  def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
    if (xs.isEmpty) m.unit
    else m.add(xs.head, sum(xs.tail))

}
