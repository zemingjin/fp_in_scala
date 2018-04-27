package chapter_5

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {
  val mockStream: Stream.Cons[Int] = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons(5, Stream.empty)))))

  it should "return a list of the given stream" in {
    assert(mockStream.toList.toString() == "List(1, 2, 3, 4, 5)")
  }

  it should "return a stream containing the n elements of the stream" in {
    assert(mockStream.take(3).size == 3)
  }

  it should "return a stream" in {
    assert(mockStream.drop(2).toString == "Stream(3, 4, 5)")
  }

  it should "return filtered stream" in {
    assert(mockStream.takeWhile(i => i == 3).toString == "")
  }

}
