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

    assert(set.map(_ + 2).toString == "Set(5, 6, 7, 3, 8, 4)")
    assert(fruits.filter(_.startsWith("app")).toString == "Set(apple)")
    assert(set.map(_ / 2).toString == "Set(0, 1, 2, 3)")

    assert(romanNumerals.toString == "Map(I -> 1, V -> 5, X -> 10)")
    assert(capitalOfCountry.toString == "Map(US -> Washington, Switzerland -> Bern)")

    assert(showCapital("US") == "Washington")
    assert(showCapital("XX") == "No capital found for 'XX'")
    val cap = capitalOfCountry withDefaultValue "<unknown>"

    assert(cap("XX") == "<unknown>")

    val fruit = List("apple", "pear", "orange", "pineapple")
    assert(fruit.sortWith(_.length < _.length).toString == "List(pear, apple, orange, pineapple)")
    assert(fruit.sorted.toString == "List(apple, orange, pear, pineapple)")
    assert(fruit.groupBy(_.head).toString == "Map(p -> List(pear, pineapple), a -> List(apple), o -> List(orange))")
  }

  it should "return a code value" in {
    assert(charCode.toString == "Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -> 5, U -> 8, F -> 3, A -> 2, " +
      "M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5, B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, " +
      "O -> 6, D -> 3, Z -> 9, S -> 7)")
    assert(wordCode("Java") == "5282")
    assert(encode("5282").toString == "Set(List(Java), List(lava))")
    assert(encode("7225247386").toString == "Set(List(rack, ah, re, to), " +
      "List(sack, ah, re, to), List(Scala, ire, to), List(sack, air, fun), List(rack, air, fun), " +
      "List(rack, bird, to), List(pack, air, fun), List(pack, ah, re, to), List(pack, bird, to), " +
      "List(Scala, is, fun), List(sack, bird, to))")
    assert(transkate("7225247386").toString == "Set(sack air fun, pack ah re to, pack bird to, Scala ire to, " +
      "Scala is fun, rack ah re to, pack air fun, sack bird to, rack bird to, sack ah re to, rack air fun)")
  }

}
