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


}
