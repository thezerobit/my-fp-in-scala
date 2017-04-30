package exercises

import org.scalatest.FlatSpec

import Chapter2._

class Chapter2Spec extends FlatSpec {
  "fib" should "calculate fib values" in {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(12) == 144)
  }

  "isSorted" should "tell whether an Array is sorted" in {
    assert(isSorted[Int](Array(), (x,y) => x <= y))
    assert(isSorted[Int](Array(1), (x,y) => x <= y))
    assert(isSorted[Int](Array(1, 2), (x,y) => x <= y))
    assert(isSorted[Int](Array(1, 2, 3), (x,y) => x <= y))
    assert(!isSorted[Int](Array(2, 1), (x,y) => x <= y))
    assert(!isSorted[Int](Array(2, 1, 3), (x,y) => x <= y))
    assert(!isSorted[Int](Array(1, 3, 2), (x,y) => x <= y))
  }

  "curry" should "curry" in {
    assert(curry((x: Int, y: Int) => x + y)(1)(2) == 3)
  }

  "uncurry" should "uncurry" in {
    def curriedPlus(x: Int)(y: Int) = x + y
    assert(uncurry(curriedPlus)(1,2) == 3)
  }

  "compose" should "compose" in {
    assert(compose((x: Int) => x * 3, (x: Int) => x + 10)(5) == 45)
  }

}
