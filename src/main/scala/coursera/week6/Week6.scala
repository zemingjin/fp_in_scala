package coursera.week6

object Week6 {
  val array = Array(1, 2, 3, 44)

  val s = "Hello World"

  def toString[T](a: Array[T]): String = s"Array(${a.foldLeft("")((t, u) => t + u.toString + ",")})"

  val l = List(1, 2, 3)

  def combs(M: Int, N: Int): List[(Int, Int)] = (1 to M).flatMap(x => (1 to N).map(y => (x, y))).toList

  def scalarProduct(l1: Vector[Double], l2: Vector[Double]): Double = (l1 zip l2).map{ case (x, y) => x * y }.sum

  def isPrime(N: Int): Boolean = (2 until N).forall(n => N % n != 0)

  def primePairs(N: Int): IndexedSeq[(Int, Int)] =
    for {
      i <- 1 until N
      j <- 1 until N
      if isPrime(i + j) && i < j
    } yield (i, j)

  def scalarProductViaFor(l1: Vector[Double], l2: Vector[Double]): Double = (for ((x, y) <- l1 zip l2) yield x * y).sum
}
