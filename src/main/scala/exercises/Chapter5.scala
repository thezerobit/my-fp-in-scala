package exercises

/**
  * Created by steve on 5/9/17.
  */
object Chapter5 {
  import Stream._
  trait Stream[+A] {
    // 5.1
    def toList: List[A] = this match {
      case Cons(head, tail) => head() :: tail().toList
      case Empty => Nil
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
      this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
        case _ => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

    @annotation.tailrec
    final def find(f: A => Boolean): Option[A] = this match {
      case Empty => None
      case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }
    // 5.2
    def take(n: Int): Stream[A] =
      if (n > 0) this match {
        case Cons(head, tail) => Cons(head, () => tail().take(n - 1))
        case Empty => Empty
      } else Empty

    def drop(n: Int): Stream[A] =
      if (n > 0) this match {
        case Cons(_, tail) => tail().drop(n - 1)
        case Empty => Empty
      } else this

    // 5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(head, tail) =>
        if (p(head())) Cons(head, () => tail().takeWhile(p))
        else tail().takeWhile(p)
      case Empty => Empty
    }

    // 5.4
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    // 5.5
    def takeWhile2(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty[A])

    // 5.6
    def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

    // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
    // writing your own function signatures.
    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((a, b) => cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

    def append[B >: A](other: => Stream[B]): Stream[B] =
      foldRight(other)((a, b) => cons(a, b))

    def flatMap[B >: A](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((a, b) => f(a).append(b))


    def startsWith[B](s: Stream[B]): Boolean = ???
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = Stream.cons(1, ones)

    // 5.8
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // 5.9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    // 5.10
    def fibs: Stream[Int] = {
      def fib(p1: Int, p2: Int): Stream[Int] = {
        val next = p1 + p2
        cons(next, fib(p2, next))
      }
      cons(0, cons(1, fib(0, 1)))
    }

    // 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((nextVal, nextState)) => cons(nextVal, unfold(nextState)(f))
        case None => empty[A]
      }

  }

}
