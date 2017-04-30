package exercises

import book_code.list._
import exercises.Chapter3._
import org.scalatest.FlatSpec

class Chapter3Spec extends FlatSpec {
  "answer_3_1" should "match the correct answer" in {
    assert(answer_3_1 == List.x)
  }

  "tail" should "return the tail of the List, if there is one" in {
    assert(tail(List(1,2,3)) == List(2,3))
    assert(tail(Nil) == Nil)
  }

  "setHead" should "replace the first element in a List or add if list is empty" in {
    assert(setHead(List(1,2,3), 0) == List(0,2,3))
    assert(setHead(Nil, 0) == List(0))
  }

  "drop" should "drop as many elements as specified or until reaching Nil" in {
    assert(drop(List(1,2,3), 1) == List(2,3))
    assert(drop(List(1,2,3), 2) == List(3))
    assert(drop(List(1,2,3), 3) == Nil)
    assert(drop(List(1,2,3), 4) == Nil)
  }

  "dropWhile" should "drop it while its hot" in {
    assert(dropWhile(List(1,2,3), (x: Int) => x < 3) == List(3))
    assert(dropWhile(List(1,2,3), (x: Int) => x > 3) == List(1,2,3))
    assert(dropWhile(List(1,2,3), (x: Int) => x < 4) == Nil)
  }

  "init" should "drop the last element from a list, if there is one" in {
    assert(init(List(1,2,3,4,5,6,7,8)) == List(1,2,3,4,5,6,7))
    assert(init(Nil) == Nil)
  }

  "length" should "give the length of a list" in {
    assert(length(List(1,2,3)) == 3)
    assert(length(Nil) == 0)
  }

  "myFoldLeft" should "fold like a champ" in {
    assert(myFoldLeft(List(1,2,3), 0)(_ + _) == 6)
  }
}
