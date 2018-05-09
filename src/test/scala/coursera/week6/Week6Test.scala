package coursera.week6

import org.scalatest.{FlatSpec, Matchers}

class Week6Test extends FlatSpec with Matchers {
  import coursera.week6.Week6._

  val dl1 = Vector(1.0, 2.0, 3.0)
  val dl2 = Vector(1.0, 2.0, 3.0, 4.0)

  it should "return a value" in {
    assert(Week6.toString(array.map(n => n * 2)) == "Array(2,4,6,88,)")
    assert(s.filter(c => c.isUpper) == "HW")
    assert(s.exists(c => c.isUpper))
    assert(!s.forall(c => c.isUpper))
    val temp = l.zip(s)
    assert(temp.toString == "List((1,H), (2,e), (3,l))")
    assert(temp.unzip.toString == "(List(1, 2, 3),List(H, e, l))")

    assert(s.flatMap(c => List('.', c)).toString == ".H.e.l.l.o. .W.o.r.l.d")

    assert(combs(3, 4).toString == "List((1,1), (1,2), (1,3), (1,4), (2,1), (2,2), (2,3), (2,4), (3,1), (3,2), (3,3), (3,4))")
    assert(scalarProduct(dl1, dl2) == 14.0)
    assert(scalarProductViaFor(dl1, dl2) == 14.0)

    assert(isPrime(7) && isPrime(11) && isPrime(13))
    assert(!isPrime(6))
    assert(primePairs(7).toString == "Vector((1,2), (1,4), (1,6), (2,3), (2,5), (3,4), (5,6))")
  }

}
