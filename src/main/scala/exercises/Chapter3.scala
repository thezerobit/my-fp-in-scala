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
  def foldLeft[A,B](list: List[A], initial: B)(f: (B, A) => B): B = list match {
    case Cons(first, rest) => foldLeft(rest,f(initial, first))(f)
    case Nil => initial
  }

  // 3.11
  def mySum(list: List[Int]): Int = foldLeft(list, 0)(_ + _)

  def myProduct(list: List[Int]): Int = foldLeft(list, 1)(_ * _)

  def myLength(list: List[Int]): Int = foldLeft(list, 0)((count, _) => count + 1)

  // 3.12
  def reverse[A](list: List[A]): List[A] = {
    @annotation.tailrec
    def _reverse(l: List[A], accum: List[A]): List[A] = l match {
      case Cons(first, rest) => _reverse(rest, Cons(first, accum))
      case Nil => accum
    }
    _reverse(list, Nil)
  }

  def reverse2[A](list: List[A]): List[A] = {
    foldLeft[A,List[A]](list, Nil)((b, a) => Cons(a, b))
  }

  // 3.13
  def foldLeft2[A,B](list: List[A], initial: B)(f: (B, A) => B): B = {
    foldRight(reverse(list), initial)((a, b) => f(b, a))
  }

  def foldRight2[A,B](list: List[A], initial: B)(f: (A, B) => B): B = {
    foldLeft(reverse(list), initial)((b, a) => f(a, b))
  }

  // 3.14
  def append2[A](l1: List[A], l2: List[A]): List[A] = {
    foldLeft(reverse(l1), l2)((b, a) => Cons(a, b))
  }

  // 3.15
  def concatenate[A](list: List[List[A]]): List[A] = {
    foldRight2[List[A], List[A]](list, Nil)((a, b) => append2(a, b))
  }

  // 3.16
  def addOneToEach(list: List[Int]): List[Int] = {
    foldRight2[Int,List[Int]](list, Nil)((a, b) => Cons(a + 1, b))
  }

  // 3.17
  def flarp(list: List[Double]): List[String] = {
    foldRight2[Double, List[String]](list, Nil)((a, b) => Cons(a.toString, b))
  }

  // 3.18
  def map[A,B](list: List[A])(f: A => B): List[B] = {
    foldRight2[A, List[B]](list, Nil)((a, b) => Cons(f(a), b))
  }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight2[A, List[A]](reverse(as), Nil)((a, b) => if (f(a)) Cons(a, b) else b)
  }

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight2[A, List[B]](as, Nil)((a, b) => append2(f(a), b))
  }

  // 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => if (f(x)) List(x) else Nil)
  }

  // 3.22
  def addElems(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (Cons(a, as), Cons(b, bs)) => Cons(a + b, addElems(as, bs))
      case _ => Nil
    }
  }

  // 3.23
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    (l1, l2) match {
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
      case _ => Nil
    }
  }

}
