package exercises

import book_code.list._
import book_code.list.List._

object Chapter3 {
  // 3.1
  val answer_3_1 = 3

  // 3.2
  def tail[A](list: List[A]): List[A] = list match {
    case Cons(_, rest) => rest
    case Nil => Nil
  }

  // 3.3
  def setHead[A](list: List[A], elem: A): List[A] = list match {
    case Cons(_, rest) => Cons(elem, rest)
    case Nil => List(elem)
  }

  // 3.4
  @annotation.tailrec
  def drop[A](list: List[A], n: Int): List[A] = {
    if (n <= 0) list
    else list match {
      case Cons(_, rest) => drop(rest, n - 1)
      case Nil => Nil
    }
  }

  // 3.5
  @annotation.tailrec
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Cons(first, rest) => if (f(first)) dropWhile(rest, f) else list
    case Nil => Nil
  }

  // 3.6
  def init[A](list: List[A]): List[A] = {
    @annotation.tailrec
    def reverse(l: List[A], accum: List[A]): List[A] = l match {
      case Cons(first, rest) => reverse(rest, Cons(first, accum))
      case Nil => accum
    }
    val reversed = reverse(list, Nil)
    reversed match {
      case Cons(_, rest) => reverse(rest, Nil)
      case Nil => Nil
    }
  }

  // 3.7
  // no, foldRight itself has no mechanism for short-circuiting

  // 3.8
  def seeWhatHappens(): Unit = {
    println(foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_)))
  }
  // The application of the provided function in foldRight correlates to
  // each Cons construction

  // 3.9
  def length[A](list: List[A]): Int = foldRight(list, 0)((_,y) => 1 + y)

  // 3.10
  @annotation.tailrec
  def myFoldLeft[A,B](list: List[A], initial: B)(f: (B, A) => B): B = list match {
    case Cons(first, rest) => myFoldLeft(rest,f(initial, first))(f)
    case Nil => initial
  }
}
