package exercises

object Chapter3 {

  sealed trait List[+A]

  // `List` data type, parameterized on a type, `A`
  case object Nil extends List[Nothing]

  // A `List` data constructor representing the empty list
  /* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
  which may be `Nil` or another `Cons`.
   */
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    // `List` companion object. Contains functions for creating and working with lists.
    def sum(ints: List[Int]): Int = ints match {
      // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    val _3_1 = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t, a2))
      }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

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
      println(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
    }

    // The application of the provided function in foldRight correlates to
    // each Cons construction

    // 3.9
    def length[A](list: List[A]): Int = foldRight(list, 0)((_, y) => 1 + y)

    // 3.10
    @annotation.tailrec
    def foldLeft[A, B](list: List[A], initial: B)(f: (B, A) => B): B = list match {
      case Cons(first, rest) => foldLeft(rest, f(initial, first))(f)
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
      foldLeft[A, List[A]](list, Nil)((b, a) => Cons(a, b))
    }

    // 3.13
    def foldLeft2[A, B](list: List[A], initial: B)(f: (B, A) => B): B = {
      foldRight(reverse(list), initial)((a, b) => f(b, a))
    }

    def foldRight2[A, B](list: List[A], initial: B)(f: (A, B) => B): B = {
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
      foldRight2[Int, List[Int]](list, Nil)((a, b) => Cons(a + 1, b))
    }

    // 3.17
    def flarp(list: List[Double]): List[String] = {
      foldRight2[Double, List[String]](list, Nil)((a, b) => Cons(a.toString, b))
    }

    // 3.18
    def map[A, B](list: List[A])(f: A => B): List[B] = {
      foldRight2[A, List[B]](list, Nil)((a, b) => Cons(f(a), b))
    }

    // 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      foldRight2[A, List[A]](reverse(as), Nil)((a, b) => if (f(a)) Cons(a, b) else b)
    }

    // 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
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
    def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
      (l1, l2) match {
        case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
        case _ => Nil
      }
    }

    // 3.24
    @annotation.tailrec
    def startsWith[A](list: List[A], sub: List[A]): Boolean = {
      (list, sub) match {
        case (Cons(a, as), Cons(b, bs)) if a == b => startsWith(as, bs)
        case (_, Nil) => true
        case _ => false
      }
    }

    @annotation.tailrec
    def hasSubsequence[A](list: List[A], sub: List[A]): Boolean = {
      list match {
        case Cons(_, as) => if (startsWith(list, sub)) true else hasSubsequence(as, sub)
        case Nil => sub == Nil
      }
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    // 3.25
    def size[A](tree: Tree[A]): Int = tree match {
      case Branch(l, t) => 1 + size(l) + size(t)
      case Leaf(_) => 1
    }

    // 3.26
    def max(tree: Tree[Int]): Int = tree match {
      case Leaf(x) => x
      case Branch(l, r) => max(l) max max(r)
    }

    // 3.27
    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    // 3.28
    def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    // 3.29
    def fold[A,B](tree: Tree[A])(f: A => B)(c: (B, B) => B): B = tree match {
      case Leaf(x) => f(x)
      case Branch(l, r) => c(fold(l)(f)(c), fold(r)(f)(c))
    }

    def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)

    def max2(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)

    def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ max _ + 1)

    def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] =
      fold[A,Tree[B]](tree)(x => Leaf(f(x)))((l, r) => Branch(map2(l)(identity), map2(r)(identity)))
  }

}
