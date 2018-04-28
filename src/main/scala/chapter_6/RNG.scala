package chapter_6

trait RNG {
  def nextInt: (Int, RNG)
  def nextDouble: (Double, RNG)

  /**
    * Exercise 6.1 - Write a function that uses RNG.nextInt to generate a random integer between 0 and
    * Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
    * Int.MinValue, which doesn’t have a non-negative counterpart.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, next): (Int, RNG) = nextInt
    (math.abs(if (n == Int.MinValue) n + 1 else n), next)
  }

  /**
    * Exercise 6.2 - Write a function to generate a Double between 0 and 1, not including 1. Note: You can
    * use Int.MaxValue to obtain the maximum positive integer value, and you can use
    * x.toDouble to convert an x: Int to a Double.
    */
  def double(rng: RNG): (Double, RNG) = rng.nextDouble

  /**
    * Exercise 6.3 - Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
    * (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
    * already written.
    */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, next_i): (Int, RNG) = rng.nextInt
    val (d, next_d): (Double, RNG) = next_i.nextDouble
    ((i, d), next_d)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, next_d): (Double, RNG) = rng.nextDouble
    val (i, next_i): (Int, RNG) = next_d.nextInt
    ((d, i), next_i)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, next1): (Double, RNG) = rng.nextDouble
    val (d2, next2): (Double, RNG) = next1.nextDouble
    val (d3, next3): (Double, RNG) = next2.nextDouble
    ((d1, d2, d3), next3)
  }

  /**
    * Exercise 6.4 - Write a function to generate a list of random integers.
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (count > 0) {
        val (n, next) = rng.nextInt
        go(count - 1, acc ::: List(n))(next)
      }
      else (acc, rng)
    }
    go(count, List())(rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /**
    * Exercise 6.5 - Use map to reimplement double in a more elegant way.
    */
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /**
    *
    */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {

  }
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  override def nextDouble: (Double, RNG) = {
    val (n, next): (Int, RNG) = nextInt
    (n / (Int.MaxValue.toDouble + 1), next)
  }
}