package chapter_5

object Chapter5 {
  def list: List[Int] = List(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3)

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue
    else onFalse

  def maybeTwice(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j + j
    else 0
  }
}
