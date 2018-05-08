package coursera.week5

import org.scalatest.{FlatSpec, Matchers}

class Week5Test extends FlatSpec with Matchers {
  val mockList = List(1, 2, 3, 4, 5)
  val mockList2 = List(6)
  val mockNested = List(mockList, mockList2, 7)
  val fluits = List("apple", "grape", "water mellon", "banana", "orange")

  it should "return the last element of the list" in {
    assert(Week5().last(mockList) == 5)
  }

  it should "return the first (n - 1) elements of the list" in {
    assert(Week5().init(mockList).toString == "List(1, 2, 3, 4)")
  }

  it should "return a concatnated list" in {
    assert(Week5().concat(mockList, mockList2).toString() == "List(1, 2, 3, 4, 5, 6)")
  }

  it should "return a reversed list" in {
    assert(Week5().reverse(mockList).toString == "List(5, 4, 3, 2, 1)")
  }

  it should "return a list minus the element at n" in {
    assert(Week5().removeAt(mockList, 3).toString == "List(1, 2, 3, 5)")
  }

  it should "return a flattened list" in {
    assert(Week5().flatten(mockNested).toString == "List(1, 2, 3, 4, 5, 6, 7)")
  }

  it should "return a sorted list" in {
    assert(Week5().msort(Week5().reverse(mockList)).toString == "List(1, 2, 3, 4, 5)")
    assert(Week5().msort(fluits).toString == "List(apple, banana, grape, orange, water mellon)")
  }

}