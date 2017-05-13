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

    // 5.13
    def map2[B](f: A => B): Stream[B] =
      unfold(this: Stream[A])({
        case Cons(h, t) => Some((f(h()), t()))
        case Empty => None
      })

    def take2(n: Int): Stream[A] =
      unfold((n, this))(s => if (s._1 > 0) s._2 match {
        case Cons(h, t) => Some(h(), (s._1 - 1, t()))
        case Empty => None
      } else None)

    def takeWhile3(p: A => Boolean): Stream[A] =
      unfold(this)({
        case Cons(h, t) => if (p(h())) Some(h(), t()) else None
        case Empty => None
      })

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold((this, s2))({
        case (Cons(h1, t1),Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case _ => None
      })

    def tailOption: Option[Stream[A]] = this match {
      case Cons(_, t) => Some(t())
      case _ => None
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
      unfold((this, s2))({
        case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()),Some(h2())), (t1(),t2())))
        case (Cons(h1, t1), Empty) => Some(((Some(h1()),None), (t1(),Empty)))
        case (Empty, Cons(h2, t2)) => Some(((None,Some(h2())), (Empty,t2())))
        case _ => None
      })

    // 5.14
    def startsWith[B](s: Stream[B]): Boolean =
      zipAll(s).filter(x => x._1 != x._2).forAll(x => x._1.isDefined && x._2.isEmpty)

    // 5.15
    def tails: Stream[Stream[A]] =
      unfold(Some(this): Option[Stream[A]])({
        case Some(s) => Some(s, s.tailOption)
        case None => None
      })

    def hasSubsequence[B >: A](s: Stream[B]): Boolean =
      tails exists (_ startsWith s)

//    def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
//      this match {
//        case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
//        case _ => z
//      }

//    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
//      this match {
//        case Cons(h,t) => val tail = t().scanRight(z)(f)
//          tail match {
//            case Cons(h2, t2) => cons(f(h(), h2()), t2())
//            case _ => ???
//          }
//        case _ => Stream(z)
//      }


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

    // 5.12
    def fibs2: Stream[Int] = unfold((0,1))(s => Some((s._1, (s._2, s._1 + s._2))))

    def from2(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

    def constant2[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

    val ones2: Stream[Int] = unfold(1)(_ => Some(1, 1))
  }
}
