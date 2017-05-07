package exercises

object Chapter4 {
  import scala.{Option => _, Some => _, None => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

  sealed trait Option[+A] {
    // 4.1
    def map[B](f: A => B): Option[B] = this match {
      case Some(x) => Some(f(x))
      case None => None
    }

    def getOrElse[B>:A](default: => B): B = this match {
      case Some(x) => x
      case None => default
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(x) => f(x)
      case None => None
    }

    def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
      case Some(x) => Some(x)
      case None => ob
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(x) => if (f(x)) Some(x) else None
      case None => None
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def failingFn(i: Int): Int = {
      val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
      try {
        val x = 42 + 5
        x + y
      }
      catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
    }

    def failingFn2(i: Int): Int = {
      try {
        val x = 42 + 5
        x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      }
      catch { case e: Exception => 43 }
    }

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    // 4.2
    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).map(m => xs.map(x => math.pow(x - m, 2))).flatMap(mean)

    // 4.3
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap(a1 => b.map(b1 => f(a1, b1)))

    // 4.4
    def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
      def seq(a: List[Option[A]], accum: List[A]): Option[List[A]] = a match {
        case Some(x) :: xs => seq(xs, x :: accum)
        case None :: _ => None
        case Nil => Some(accum)
      }
      seq(a, Nil).map(_.reverse)
    }

    // 4.5
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      @annotation.tailrec
      def trav(a: List[A], accum: List[B]): Option[List[B]] = a match {
        case x :: xs => f(x) match {
          case Some(y) => trav(xs, y :: accum)
          case None => None
        }
        case Nil => Some(accum)
      }
      trav(a, Nil).map(_.reverse)
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      traverse(a)(identity)
    }
  }

  import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

  sealed trait Either[+E,+A] {
    // 4.6
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => Right(a)
      case Left(e) => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this.flatMap(a => b.map(b => f(a, b)))
  }
  case class Left[+E](get: E) extends Either[E,Nothing]
  case class Right[+A](get: A) extends Either[Nothing,A]

  object Either {
    // 4.7
    def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      @annotation.tailrec
      def trav(list: List[A], accum: List[B]): Either[E, List[B]] = list match {
        case a :: as => f(a) match {
          case Right(b) => trav(as, b :: accum)
          case Left(e) => Left(e)
        }
        case Nil => Right(accum.reverse)
      }
      trav(es, Nil)
    }

    def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(identity)

    // 4.8
    // Some way to combine Left values (in this case appending the strings)
    // I would change map2.
    // I would use Either like this: Either[List[E],A], etc.
    // Those functions would need to possibly combine the Left values

    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty)
        Left("mean of empty list!")
      else
        Right(xs.sum / xs.length)

    def safeDiv(x: Int, y: Int): Either[Exception, Int] =
      try Right(x / y)
      catch { case e: Exception => Left(e) }

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case e: Exception => Left(e) }
  }
}
