package chapter_6

import org.scalatest.{FlatSpec, Matchers}

class RNGTest extends FlatSpec with Matchers {
  private val rng = SimpleRNG(42)

  it should "Return a number" in {
    val (n1, next1): (Int, RNG) = rng.nextInt

    assert(n1 == 16159453)
    val (n2, next2): (Int, RNG) = next1.nextInt
    assert(n2 == -1281479697)
  }

  it should "Return only positive nunbers" in {
    val (n1, next1): (Int, RNG) = rng.nonNegativeInt(rng)
    assert(n1 == 16159453)
    val (n2, next2): (Int, RNG) = next1.nonNegativeInt(next1)
    assert(n2 == 1281479697)
  }

  it should "Return only double nunbers" in {
    val (n1, next1): (Double, RNG) = rng.double(rng)
    assert(n1 == 0.007524831686168909)
    val d: rng.Rand[Double] = rng.doubleViaMap
    assert(d.toString == "")
    val (n2, next2): (Double, RNG) = next1.double(next1)
    assert(n2 == -0.5967354853637516)
  }

  it should "return mixed tuple of double and int" in {
    val ((i, d), n1) = rng.intDouble(rng)
    assert(rng.intDouble(rng).toString() == "((16159453,-0.5967354853637516),SimpleRNG(197491923327988))")
    val ((d2, i2), n2) = rng.doubleInt(n1)
    assert(rng.doubleInt(n1).toString == "((-0.15846728440374136,-2015756020),SimpleRNG(149370390209998))")
    assert(rng.double3(n2).toString() == "((0.8242210922762752,-0.9008632316254079,0.4730720594525337),SimpleRNG(66578973461475))")
  }

  it should "return a list of random ints" in {
    assert(rng.ints(4)(rng).toString() == "(List(16159453, -1281479697, -340305902, -2015756020),SimpleRNG(149370390209998))")
  }

}
