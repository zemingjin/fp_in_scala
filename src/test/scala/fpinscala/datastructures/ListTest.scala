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
}
