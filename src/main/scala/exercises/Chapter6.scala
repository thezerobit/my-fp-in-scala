package exercises

object Chapter6 {
  def buildStream[A,B](initial: A)(f: A => (B, A)): Stream[B] =
    f(initial) match {
      case (b, a) => Stream.cons(b, buildStream(a)(f))
    }

  trait RNG {
    def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
  }

  object RNG {
    // NB - this was called SimpleRNG in the book text

    case class Simple(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
        val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
        val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
        (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      }
    }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    // 6.1
    def nonNegativeInt(rng: RNG): (Int, RNG) =
      rng.nextInt match {
        case (Int.MinValue, rngNext1) => nonNegativeInt(rngNext1)
        case (x: Int, rngNext2) => (math.abs(x), rngNext2)
      }

    // 6.2
    def double(rng: RNG): (Double, RNG) =
      rng.nextInt match {
        case (Int.MinValue, rngNext1) => double(rngNext1)
        case (x: Int, rngNext2) =>
          (math.abs(x).toDouble / (Int.MaxValue.toLong + 1).toDouble, rngNext2)
      }

    // 6.3
    def intDouble(rng: RNG): ((Int,Double), RNG) = {
      val (intVal, rng2) = rng.nextInt
      val (doubleVal, rng3) = double(rng2)
      ((intVal, doubleVal), rng3)
    }

    def doubleInt(rng: RNG): ((Double,Int), RNG) =
      intDouble(rng) match {
        case ((intVal, doubleVal), rng2) => ((doubleVal, intVal), rng2)
      }

    def double3(rng: RNG): ((Double,Double,Double), RNG) = {
      val (doubleVal, rng2) = double(rng)
      val (doubleVal2, rng3) = double(rng2)
      val (doubleVal3, rng4) = double(rng3)
      ((doubleVal, doubleVal, doubleVal), rng4)
    }

    // 6.4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      def ints_(cnt: Int, rng2: RNG, accum: List[Int]): (List[Int], RNG) =
        if (cnt > 0) rng2.nextInt match {
          case (intVal, rng3) => ints_(cnt - 1, rng3, intVal :: accum)
        } else (accum, rng2)
      ints_(count, rng, Nil)
    }

    // 6.5
    def double2(rng: RNG): (Double, RNG) =
      map(int)(x =>
        if (x == Int.MinValue) 0.0
        else math.abs(x).toDouble / (Int.MaxValue.toLong + 1).toDouble)(rng)

    // 6.6
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      (rng: RNG) => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a,b), rng3)
      }

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
  }

  case class State[S,+A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      ???
    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      ???
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      ???
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object State {
    type Rand[A] = State[RNG, A]
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
  }
}
