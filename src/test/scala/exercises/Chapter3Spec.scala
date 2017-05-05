package exercises

import exercises.Chapter3._
import org.scalatest.FlatSpec

class Chapter3Spec extends FlatSpec {

  "List.answer_3_1" should "match the correct answer" in {
    assert(List.answer_3_1 == List._3_1)
  }

  "List.tail" should "return the tail of the List, if there is one" in {
    assert(List.tail(List(1,2,3)) == List(2,3))
    assert(List.tail(Nil) == Nil)
  }

  "List.setHead" should "replace the first element in a List or add if list is empty" in {
    assert(List.setHead(List(1,2,3), 0) == List(0,2,3))
    assert(List.setHead(Nil, 0) == List(0))
  }

  "List.drop" should "drop as many elements as specified or until reaching Nil" in {
    assert(List.drop(List(1,2,3), 1) == List(2,3))
    assert(List.drop(List(1,2,3), 2) == List(3))
    assert(List.drop(List(1,2,3), 3) == Nil)
    assert(List.drop(List(1,2,3), 4) == Nil)
  }

  "List.dropWhile" should "drop it while its hot" in {
    assert(List.dropWhile(List(1,2,3), (x: Int) => x < 3) == List(3))
    assert(List.dropWhile(List(1,2,3), (x: Int) => x > 3) == List(1,2,3))
    assert(List.dropWhile(List(1,2,3), (x: Int) => x < 4) == Nil)
  }

  "List.init" should "drop the last element from a list, if there is one" in {
    assert(List.init(List(1,2,3,4,5,6,7,8)) == List(1,2,3,4,5,6,7))
    assert(List.init(Nil) == Nil)
  }

  "List.length" should "give the length of a list" in {
    assert(List.length(List(1,2,3)) == 3)
    assert(List.length(Nil) == 0)
  }

  "List.foldLeft" should "fold like a champ" in {
    assert(List.foldLeft(List(1,2,3), 0)(_ + _) == 6)
    assert(List.foldLeft(List("a","b","c"), "")(_ + _) == "abc")
  }

  "List.mySum" should "sum a list of integers" in {
    assert(List.mySum(List(1,2,3,4)) == 10)
    assert(List.mySum(Nil) == 0)
  }

  "List.myProduct" should "calculate the product of a list of integers" in {
    assert(List.myProduct(List(1,2,3)) == 6)
    assert(List.myProduct(Nil) == 1)
  }

  "List.myLength" should "calculate the length of a list" in {
    assert(List.myLength(List(1,2,3)) == 3)
    assert(List.myLength(Nil) == 0)
  }

  "List.reverse" should "reverse a list" in {
    assert(List.reverse(List(1,2,3)) == List(3,2,1))
  }

  "List.foldLeft2" should "calculate foldLeft in terms of foldRight" in {
    assert(List.foldLeft2(List("a","b","c"), "")(_ + _) == "abc")
  }

  "List.foldRight2" should "calculate foldRight in terms of foldLeft" in {
    assert(List.foldRight2(List("a","b","c"), "")(_ + _) == "abc")
  }

  "List.append2" should "append 2 lists" in {
    assert(List.append2(List(1,2,3), List(4,5,6)) == List(1,2,3,4,5,6))
  }

  "List.concatenate" should "append a list of lists into one list" in {
    assert(List.concatenate(List(List(1,2,3), List(4,5,6), List(7,8,9))) == List(1,2,3,4,5,6,7,8,9))
  }

  "List.addOneToEach" should "add 1 to each element of a list" in {
    assert(List.addOneToEach(List(1,2,3)) == List(2,3,4))
  }

  "List.flarp" should "convert a list of Double to a list of String" in {
    assert(List.flarp(List(1.0, 2.2)) == List("1.0", "2.2"))
  }

  "List.map" should "map" in {
    assert(List.map(List(1,2,3))(x => x * 2) == List(2,4,6))
  }

  "List.filter" should "return a list with items that satisfy predicate" in {
    assert(List.filter(List(1,2,3))(x => x % 2 == 0) == List(2))
  }

  "List.flatMap" should "map, flatly" in {
    assert(List.flatMap(List(1,2,3))(x => List(x, -x)) == List(1,-1,2,-2,3,-3))
  }

  "List.filter2" should "return a list with items that satisfy predicate" in {
    assert(List.filter2(List(1,2,3))(x => x % 2 == 0) == List(2))
  }

  "List.addElems" should "combine lists of numbers elementwise with addition" in {
    assert(List.addElems(List(1,2,3), List(10, 20, 30)) == List(11, 22, 33))
  }

  "List.zipWith" should "combine lists with an arbitrary binary function" in {
    assert(List.zipWith(List(1,2,3), List(10,20,30))(_ * _) == List(10, 40, 90))
  }

  "List.hasSubsequence" should "detect if a list contains another as a subsequence" in {
    assert(List.hasSubsequence(Nil, Nil))
    assert(List.hasSubsequence(List(1,2,3), Nil))
    assert(List.hasSubsequence(List(1,2,3,4,5), List(1)))
    assert(List.hasSubsequence(List(1,2,3,4,5), List(1,2,3)))
    assert(List.hasSubsequence(List(1,2,3,4,5), List(2,3,4)))
    assert(List.hasSubsequence(List(1,2,3,4,5), List(4,5)))
    assert(List.hasSubsequence(List(1,2,3,4,5), List(4)))
    assert(List.hasSubsequence(List(1,2,3,4,5), List(5)))
    assert(!List.hasSubsequence(List(1,2,3), List(3,2)))
    assert(!List.hasSubsequence(List(1,2,3), List(4)))
    assert(!List.hasSubsequence(List(1,2,3), List(1,2,3,4)))
  }

  "Tree.size" should "count the leaves and branches" in {
    assert(Tree.size(Branch(Leaf(1), Leaf(2))) == 3)
    assert(Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 5)
  }

  "Tree.max" should "find the largest value in tree" in {
    assert(Tree.max(Branch(Leaf(1), Leaf(2))) == 2)
    assert(Tree.max(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))) == 3)
  }

  "Tree.depth" should "find the maximum path length from root to leaf" in {
    assert(Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)
  }



}
