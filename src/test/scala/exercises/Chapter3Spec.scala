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

  "foldLeft" should "fold like a champ" in {
    assert(foldLeft(List(1,2,3), 0)(_ + _) == 6)
    assert(foldLeft(List("a","b","c"), "")(_ + _) == "abc")
  }

  "mySum" should "sum a list of integers" in {
    assert(mySum(List(1,2,3,4)) == 10)
    assert(mySum(Nil) == 0)
  }

  "myProduct" should "calculate the product of a list of integers" in {
    assert(myProduct(List(1,2,3)) == 6)
    assert(myProduct(Nil) == 1)
  }

  "myLength" should "calculate the length of a list" in {
    assert(myLength(List(1,2,3)) == 3)
    assert(myLength(Nil) == 0)
  }

  "reverse2" should "reverse a list" in {
    assert(reverse(List(1,2,3)) == List(3,2,1))
  }

  "foldLeft2" should "calculate foldLeft in terms of foldRight" in {
    assert(foldLeft2(List("a","b","c"), "")(_ + _) == "abc")
  }

  "foldRight2" should "calculate foldRight in terms of foldLeft" in {
    assert(foldRight2(List("a","b","c"), "")(_ + _) == "abc")
  }

  "append2" should "append 2 lists" in {
    assert(append2(List(1,2,3), List(4,5,6)) == List(1,2,3,4,5,6))
  }

  "concatenate" should "append a list of lists into one list" in {
    assert(concatenate(List(List(1,2,3), List(4,5,6), List(7,8,9))) == List(1,2,3,4,5,6,7,8,9))
  }

  "addOneToEach" should "add 1 to each element of a list" in {
    assert(addOneToEach(List(1,2,3)) == List(2,3,4))
  }

  "flarp" should "convert a list of Double to a list of String" in {
    assert(flarp(List(1.0, 2.2)) == List("1.0", "2.2"))
  }

  "map" should "map" in {
    assert(map(List(1,2,3))(x => x * 2) == List(2,4,6))
  }

  "filter" should "return a list with items that satisfy predicate" in {
    assert(filter(List(1,2,3))(x => x % 2 == 0) == List(2))
  }

  "flatMap" should "map, flatly" in {
    assert(flatMap(List(1,2,3))(x => List(x, -x)) == List(1,-1,2,-2,3,-3))
  }

  "filter2" should "return a list with items that satisfy predicate" in {
    assert(filter2(List(1,2,3))(x => x % 2 == 0) == List(2))
  }

  "addElems" should "combine lists of numbers elementwise with addition" in {
    assert(addElems(List(1,2,3), List(10, 20, 30)) == List(11, 22, 33))
  }

  "zipWith" should "combine lists with an arbitrary binary function" in {
    assert(zipWith(List(1,2,3), List(10,20,30))(_ * _) == List(10, 40, 90))
  }

}
