package chapter_7

import chapter_7.Par.Par

object Chapter7 {
  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    println(s"sum(${ints.toString})")
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }
}
