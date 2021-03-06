package exercises

import org.scalatest.FlatSpec

class Chapter4Spec extends FlatSpec {
  import scala.{Option => _, Some => _, None => _, Either => _, Left => _, Right => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

  import exercises.Chapter4._

  "Option.map" should "map" in {
    assert(Some(1).map(_ + 1) == Some(2))
  }

  "Option.getOrElse" should "get the value or the one provided" in {
    assert(Some(1).getOrElse(2) == 1)
    assert(None.getOrElse(2) == 2)
  }

  "Option.flatMap" should "map, flatly" in {
    assert(Some(1).flatMap(x => Some(x + 1)) == Some(2))
    assert((None: Option[Int]).flatMap(x => Some(x + 1)) == None)
    assert(Some(1).flatMap(x => None) == None)
  }

  "Option.orElse" should "return the Option if present or the provided one" in {
    assert(Some(1).orElse(Some(2)) == Some(1))
    assert(None.orElse(Some(2)) == Some(2))
  }

  "Option.filter" should "return None if contained object does not satisfy predicate" in {
    assert(Some(1).filter(_ % 2 == 0) == None)
    assert(Some(2).filter(_ % 2 == 0) == Some(2))
    assert((None: Option[Int]).filter(_ % 2 == 0) == None)
  }

  "Option.variance" should "calculate variance of a Sequence" in {
    assert(Option.variance(Seq(1,2,3)) == Some(2.0/3.0))
    assert(Option.variance(Seq.empty) == None)
  }

  "Option.map2" should "allow a binary function to operate on 2 Optional values" in {
    assert(Option.map2(Some(1), Some(2))(_ + _) == Some(3))
    assert(Option.map2(None: Option[Int], Some(2))(_ + _) == None)
    assert(Option.map2(Some(1), None)(_ + _) == None)
    assert(Option.map2(None: Option[Int], None)(_ + _) == None)
  }

  "Option.sequence2" should "convert a list of Options to an Optional list..." in {
    assert(Option.sequence2(List(Some(1), Some(2))) == Some(List(1,2)))
    assert(Option.sequence2(List(None, Some(2))) == None)
    assert(Option.sequence2(List(Some(2), None)) == None)
    assert(Option.sequence2(Nil) == Some(Nil))
  }

  "Option.sequence" should "..." in {
    assert(Option.sequence(List(Some(1), Some(2))) == Some(List(1,2)))
    assert(Option.sequence(List(None, Some(2))) == None)
    assert(Option.sequence(List(Some(2), None)) == None)
    assert(Option.sequence(Nil) == Some(Nil))
  }

  "Either.map" should "map over Right value" in {
    assert(Right(1).map(_ * 2) == Right(2))
    assert((Left(1): Either[Int, Int]).map(_ * 2) == Left(1))
  }

  "Either.flatMap" should "map, flatly, over Right value" in {
    assert(Right(1).flatMap(x => Right(x * 2)) == Right(2))
    assert((Left(1): Either[Int, Int]).flatMap(x => Right(x * 2)) == Left(1))
    assert(Right(1).flatMap(x => Left(x * 2)) == Left(2))
    assert((Left(1): Either[Int, Int]).flatMap(x => Left(x * 2)) == Left(1))
  }

  "Either.orElse" should "return the first Right or try the next" in {
    assert(Right(1).orElse(Right(2)) == Right(1))
    assert(Left(1).orElse(Right(2)) == Right(2))
    assert(Right(1).orElse(Left(2)) == Right(1))
    assert(Left(1).orElse(Left(2)) == Left(2))
  }

  "Either.map2" should "map the right values over a binary function" in {
    assert(Right(1).map2(Right(2))(_ + _) == Right(3))
    assert((Left(1): Either[Int, Int]).map2(Right(2))(_ + _) == Left(1))
    assert(Right(1).map2(Left(2))(_ + _) == Left(2))
    assert((Left(1): Either[Int, Int]).map2(Left(2))(_ + _) == Left(1))
  }

  "Either.traverse" should "map or return the first Left" in {
    assert(Either.traverse(List(1,2,3))(x => if (x == 2) Left("sad") else Right(x * 2)) == Left("sad"))
    assert(Either.traverse(List(1,2,3))(x => if (x == 4) Left("sad") else Right(x * 2)) == Right(List(2,4,6)))
  }

  "Either.sequence" should "return List of right values or first Left" in {
    assert(Either.sequence(List(Right(1),Right(2))) == Right(List(1,2)))
    assert(Either.sequence(List(Right(1),Left(2),Left(3)): List[Either[Int,Int]]) == Left(2))
  }

}
