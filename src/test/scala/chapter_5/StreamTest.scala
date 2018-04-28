package chapter_5

import chapter_5.Stream.cons
import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {
  val mockStream: Stream[Int] = Stream(1, 2, 3, 4, 5)
  val ones: Stream[Int] = Stream.cons(1, ones)
  val onesViaUnfold: Stream[Int] = Stream.unfold(1)(a => Option(a, a))

  it should "return a list of the given stream" in {
    assert(mockStream.toString == "Stream(1, 2, 3, 4, 5)")
  }

  it should "return a stream containing the n elements of the stream" in {
    assert(mockStream.take(3).toString == "Stream(1, 2, 3)")
    assert(mockStream.takeViaUnfold(3).toString == "Stream(1, 2, 3)")
  }

  it should "return a stream" in {
    assert(mockStream.drop(2).toString == "Stream(3, 4, 5)")
  }

  it should "return filtered stream" in {
    assert(mockStream.takeWhile(i => i < 3).toString == "Stream(1, 2)")
    assert(mockStream.takeWhileViaFoldRight(i => i < 3).toString == "Stream(1, 2)")
    assert(mockStream.takeWhileViaUnfold(i => i < 3).toString == "Stream(1, 2)")
    assert(mockStream.filter(i => i < 4).toString == "Stream(1, 2, 3)")
  }

  it should "return true or false if all elements in the stream satisfies the condition" in {
    assert(mockStream.forAll(a => a <= 5))
  }

  it should "return an Option" in {
    assert(mockStream.headOption.toString == "Some(1)")
  }

  it should "return Stream[B] for map" in {
    assert(mockStream.map(a => a + a).toString == "Stream(2, 4, 6, 8, 10)")
    assert(mockStream.mapViaUnfold(a => a + a).toString == "Stream(2, 4, 6, 8, 10)")
  }

  it should "return a combined stream for append" in {
    assert(mockStream.append(mockStream).toString == "Stream(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)")
  }

  it should "return a modified stream for flatMap" in {
    assert(mockStream.flatMap(a => cons(a*3, Empty)).toString == "Stream(3, 6, 9, 12, 15)")
  }

  it should "return the one's stream" in {
    assert(ones.take(5).toString == "Stream(1, 1, 1, 1, 1)")
    assert(onesViaUnfold.take(5).toString == "Stream(1, 1, 1, 1, 1)")
  }

  it should "return an infinit stream" in {
    assert(Stream.constant(5).take(5).toString == "Stream(5, 5, 5, 5, 5)")
    assert(Stream.constantViaUnfold(5).take(5).toString == "Stream(5, 5, 5, 5, 5)")
    assert(Stream.from(5).take(5).toString == "Stream(5, 6, 7, 8, 9)")
    assert(Stream.fromViaUnfold(5).take(5).toString == "Stream(5, 6, 7, 8, 9)")
    assert(Stream.fibs.take(7).toString == "Stream(0, 1, 1, 2, 3, 5, 8)")
    assert(Stream.fibsViaUnfold.take(7).toString == "Stream(0, 1, 1, 2, 3, 5, 8)")
  }

  it should "return a stream of zipped tuple" in {
    assert(mockStream.zipWithViaUnfold(mockStream)((a, b) => a * b).toString == "Stream(1, 4, 9, 16, 25)")
    assert(mockStream.zipAllViaUnfold(mockStream).toString ==
             "Stream((Some(1),Some(1)), (Some(2),Some(2)), (Some(3),Some(3)), (Some(4),Some(4)), (Some(5),Some(5)))")
    assert(mockStream.zipAllViaUnfold(Stream(4, 5, 6)).toString ==
             "Stream((Some(1),Some(4)), (Some(2),Some(5)), (Some(3),Some(6)), (Some(4),None), (Some(5),None))")
    assert(mockStream.zipWithAllViaUnfold(Stream(4, 5, 6))((a, b) => (a, b)).toString ==
             "Stream((Some(1),Some(4)), (Some(2),Some(5)), (Some(3),Some(6)), (Some(4),None), (Some(5),None))")
  }

  it should "return a true or false for startWith" in {
    assert(mockStream.startsWithViaUnfold(Stream(1, 2, 3)))
    assert(!mockStream.startsWithViaUnfold(Stream(2, 3)))
  }

  it should "return a stream of stream from tails" in {
    assert(mockStream.tails.toString ==
             "Stream(Stream(1, 2, 3, 4, 5), Stream(2, 3, 4, 5), Stream(3, 4, 5), Stream(4, 5), Stream(5), Empty)")
    assert(mockStream.hasSubsequence(Stream(2, 3)))
    assert(!mockStream.hasSubsequence(Stream(1, 3)))
  }
}
