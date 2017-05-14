package exercises

import org.scalatest.FlatSpec

class Chapter6Spec extends FlatSpec {
  import exercises.Chapter6._

  "RNG.nonNegativeInt" should "return non-negative integers, and deal w/ Int.MinValue" in {
    object FakeRNG extends RNG {
      override def nextInt: (Int, RNG) = (Int.MinValue, RNG.Simple(69))
    }

    assert(RNG.nonNegativeInt(FakeRNG)._1 >= 0)
  }

  val values = List(Int.MinValue, Int.MaxValue, 0, Int.MinValue + 1, Int.MaxValue - 1)
  class FakeRNG(vals: List[Int]) extends RNG {
    override def nextInt: (Int, RNG) = vals match {
      case head :: tail => (head, new FakeRNG(tail))
      case Nil => RNG.Simple(69).nextInt
    }
  }

  "RNG.double" should "produce a Double from 0 up to but not including 1" in {
    buildStream(new FakeRNG(values): RNG)(RNG.double).take(10)
      .foreach(x => assert(x >= 0 && x < 1))
  }

  "RNG.ints" should "produce a list of Int and an RNG" in {
    assert(RNG.ints(10)(RNG.Simple(69))._1.length == 10)
  }

  "RNG.double2" should "produce a Double from 0 up to but not including 1" in {
    buildStream(new FakeRNG(values): RNG)(RNG.double2).take(10)
      .foreach(x => assert(x >= 0 && x < 1))
  }

}
