package exercises

object Chapter2 {

  // 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibRec(left: Int, prev: Int, prev2: Int): Int = {
      val next = prev + prev2
      left match {
        case 0 => next
        case _ => fibRec(left - 1, next, prev)
      }
    }
    n match {
      case 0 => 0
      case 1 => 1
      case x if x > 1 => fibRec(x - 2, 1, 0)
    }
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def isSortedRec(index: Int): Boolean = index match {
      case n if n >= as.length => true
      case _ => ordered(as(index - 1), as(index)) && isSortedRec(index + 1)
    }
    isSortedRec(1)
  }

  // 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

}
