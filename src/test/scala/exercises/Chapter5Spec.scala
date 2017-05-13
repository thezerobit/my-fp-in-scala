package exercises

import org.scalatest.FlatSpec

class Chapter5Spec extends FlatSpec {
  import exercises.Chapter5._

  "Stream.toList" should "convert Stream to List" in {
    assert(Stream(1,2,3).toList == List(1,2,3))
  }

  "Stream.take" should "take from the front of the stream" in {
    assert(Stream(1,2,3).take(2).toList == List(1,2))
  }

  "Stream.drop" should "drop elements from the stream" in {
    assert(Stream(1,2,3).drop(2).toList == List(3))
  }

  "Stream.takeWhile" should "take elements while the predicate is satisfied" in {
    assert(Stream(1,2,3,4,5).takeWhile(_ <= 2).toList == List(1,2))
  }

  "Stream.forAll" should "return true if all elements satisfy, returning early" in {
    import exercises.Chapter5.Stream.cons
    assert(!(cons(1, cons(2, cons(throw new Exception("blam"), Empty))): Stream[Int]).forAll(_ < 2))
  }

  "Stream.takeWhile2" should "take elements while the predicate is satisfied" in {
    assert(Stream(1,2,3,4,5).takeWhile2(_ <= 2).toList == List(1,2))
  }

  "Stream.headOption" should "return the head or None" in {
    assert(Stream().headOption.isEmpty)
    assert(Stream(1, 2, 3).headOption.contains(1))
  }

  "Stream.map" should "map lazily" in {
    assert(Stream(1,2,3).map(_ * 2).toList == List(2,4,6))
  }

  "Stream.filter" should "filter lazily" in {
    assert(Stream(1,2,3,4,5).filter(_ % 2 == 0).toList == List(2,4))
  }

  "Stream.append" should "append lazily" in {
    assert(Stream(1,2,3).append(Stream(4,5,6)).toList == List(1,2,3,4,5,6))
  }

  "Stream.flatMap" should "flatMap lazily" in {
    val s = Stream(1,2,3).flatMap(x => Stream.ones.take(x))
    assert(s.toList == List(1,1,1,1,1,1))
  }

  "Stream.constant" should "produce infinite stream of the value" in {
    assert(Stream.constant(2).take(5).toList == List(2,2,2,2,2))
  }

  "Stream.from" should "produce stream of incrementing integers" in {
    assert(Stream.from(5).take(5).toList == List(5,6,7,8,9))
  }

  "Stream.fibs" should "produce fibonacci sequence stream" in {
    assert(Stream.fibs.take(7).toList == List(0,1,1,2,3,5,8))
  }

  "Stream.unfold" should "produce streams from a initial value and function" in {
    assert(Stream.unfold(10)(x => if (x >= 0) Some(x, x - 1) else None).toList
      == List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0))
  }

  "Stream.fibs2" should "produce fibonacci sequence stream" in {
    assert(Stream.fibs2.take(7).toList == List(0,1,1,2,3,5,8))
  }

  "Stream.from2" should "produce stream of incrementing integers" in {
    assert(Stream.from2(5).take(5).toList == List(5,6,7,8,9))
  }

  "Stream.constant2" should "produce infinite stream of the value" in {
    assert(Stream.constant2(2).take(5).toList == List(2,2,2,2,2))
  }

  "Stream.ones2" should "produce a stream of 1" in {
    assert(Stream.ones2.take(4).toList == List(1,1,1,1))
  }

  "Stream.map2" should "map lazily" in {
    assert(Stream(1,2,3).map2(_ * 2).toList == List(2,4,6))
  }

  "Stream.take2" should "take from the front of the stream" in {
    assert(Stream(1,2,3).take2(2).toList == List(1,2))
  }

  "Stream.takeWhile3" should "take elements while the predicate is satisfied" in {
    assert(Stream(1,2,3,4,5).takeWhile3(_ <= 2).toList == List(1,2))
  }

  "Stream.zipWith" should "zipWith lazily" in {
    assert(Stream(1,2,3,4,5).zipWith(Stream(10,20,30))(_ + _).toList == List(11,22,33))
  }

  "Stream.zipAll" should "zipAll the values" in {
    assert(Stream(1, 2).zipAll(Stream("A", "B", "C")).toList
      == List((Some(1), Some("A")), (Some(2), Some("B")), (None, Some("C"))))
  }

  "Stream.startsWith" should "detect if a stream starts with another" in {
    assert(Stream(1,2).startsWith(Stream(1)))
    assert(Stream(1,2).startsWith(Stream(1,2)))
    assert(!Stream(1).startsWith(Stream(1,2)))
  }

  "Stream.tails" should "return the tails of a stream" in {
    assert(Stream(1,2,3).tails.map(_.toList).toList == List(List(1,2,3), List(2,3), List(3), Nil))
  }

  "Stream.hasSubsequence" should "detect if a stream is a subsequence" in {
    assert(Stream(1,2,3,4,5).hasSubsequence(Stream(2,3,4)))
    assert(!Stream(1,2,3,4,5).hasSubsequence(Stream(2,3,5)))

  }

//  "Stream.scanRight" should "scan from teh right/?" in {
//    assert(Stream(1,2,3).scanRight(0)(_ + _).toList == List(6,5,3,0))
//  }

}
