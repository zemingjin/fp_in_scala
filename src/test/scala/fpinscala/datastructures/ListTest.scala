package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {
  it should "return a value when the given pattern matches" in {
    assert(List(1,2,3) == Cons(1, Cons(2, Cons(3, Nil))))
  }

  it should "return a list with 1st item removed" in
    assert(List.tail(List(1, 2, 3)) == List(2, 3))

  it should "return a list with the given item from setHead" in
    assert(List.setHead(9, List(1, 2, 3)) == List(9, 2, 3))

  it should "return a list with n element removed from drop" in
    assert(List.drop(List(1, 2, 3, 4, 5, 6), 3) == List(4, 5, 6))

  it should "return a list with n elements remove from dropWhile" in
    assert(List.dropWhile(List(1, 2, 3, 4, 5, 6), (a: Int) => a < 4) == List(4, 5, 6))

  it should "return a list of union of the given 2 lists" in
    assert(List.append(List(1, 2, 3), List(1, 2, 3)) == List(1, 2, 3, 1, 2, 3))

  it should "return a list without the last entry for init" in
    assert(List.init(List(1, 2, 3)) == List(1, 2))

  it should "return the sum of items in the given list" in {
    assert(List.sum2(List(1, 2, 3)) == 6)
    assert(List.sum2(List(1, 2, 3, 4)) == 10)
  }

  it should "return the product of items in the given list" in {
    assert(List.product2(List(1, 2, 3)) == 6)
    assert(List.product2(List(1, 2, 3, 4)) == 24)
  }

  it should "return the length of the given list" in {
    assert(List.length(List(1, 2, 3)) == 3)
    assert(List.length(Nil) == 0)
  }

  it should "return the sum of the given list with foldLeft" in {
    assert(List.sum3(List(1, 2, 3)) == 6)
    assert(List.sum3(List(1, 2, 3, 4)) == 10)
  }

  it should "return the product of the given list with foldLeft" in {
    assert(List.product3(List(1, 2, 3)) == 6)
    assert(List.product3(List(1, 2, 3, 4)) == 24)
  }

  it should "return the length of the given list using foldLeft" in {
    assert(List.length2(List(1, 2, 3)) == 3)
    assert(List.length2(Nil) == 0)
  }

  it should "return the list with the reverse order of the given list" in {
    assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  it should "return the sum of items in the given list by foldRightViaFoldLeft" in {
    assert(List.sum4(List(1, 2, 3)) == 6)
    assert(List.sum4(List(1, 2, 3, 4)) == 10)
  }

  it should "return the product of items in the given list by foldRightViaFoldLeft" in {
    assert(List.product4(List(1, 2, 3)) == 6)
    assert(List.product4(List(1, 2, 3, 4)) == 24)
  }

  it should "return the sum of the given list with foldLeftViaFoldRight" in {
    assert(List.sum5(List(1, 2, 3)) == 6)
    assert(List.sum5(List(1, 2, 3, 4)) == 10)
  }

  it should "return the product of the given list with foldLeftViaFoldRight" in {
    assert(List.product5(List(1, 2, 3)) == 6)
    assert(List.product5(List(1, 2, 3, 4)) == 24)
  }

  it should "return the length of the given list using foldLeftViaFoldRight" in {
    assert(List.length3(List(1, 2, 3)) == 3)
    assert(List.length3(Nil) == 0)
  }

  it should "return a combined list of the given 2 lists by appendViaFoldLeft" in {
    assert(List.appendViaFoldLeft(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
  }

  it should "return a combined list of the given 2 lists by appendViaFoldRight" in {
    assert(List.appendViaFoldRight(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
  }

}
