package exercises

import org.scalatest.FlatSpec

class Chapter4Spec extends FlatSpec {
  import scala.{Option => _, Some => _, None => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
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

}
