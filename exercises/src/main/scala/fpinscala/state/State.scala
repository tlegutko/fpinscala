package fpinscala.state

import scala.collection.mutable.ListBuffer

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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt2(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (n, rng2) =>
        if (n != Int.MinValue) (n.abs, rng2)
        else (Int.MaxValue, rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if (n != Int.MinValue) (n.abs, rng2)
    else (Int.MaxValue, rng2)

  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i, rng2) => (i % 2 == 0, rng2) }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    ((-n / Int.MinValue).toDouble, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(c: Int, r: RNG, acc: ListBuffer[Int]): (List[Int], RNG) = {
      if (c > 0) {
        val (n, r2) = r.nextInt
        acc.append(n)
        go(c - 1, r2, acc)
      } else (acc.toList, r)
    }
    go(count, rng, ListBuffer.empty)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(c: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (c > 0) {
        val (n, r2) = r.nextInt
        go(c - 1, r2, n :: acc)
      } else (acc.toList, r)
    }
    go(count, rng, List.empty)
  }

  def positiveMax(n: Int): Rand[Int] =
    map(nonNegativeInt)(_ % (n + 1))

  def doubleWithMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA2) = ra(rng)
      val (b, rngB2) = rb(rngA2)
      (f(a, b), rngB2)
    }

  def intDoubleWithMap2: Rand[(Int, Double)] =
    map2(int, doubleWithMap)((a, b) => (a, b))

  def doubleIntWithMap2: Rand[(Double, Int)] =
    map2(int, doubleWithMap)((a, b) => (b, a))

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    val (l, r) = fs.foldLeft((List.empty[A], rng)) {
      case ((acc, currRng), r) => {
        val (x, r2) = r(currRng)
        (x :: acc, r2)
      }
    }
    (l.reverse, r)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((r, acc) => map2(r, acc)(_ :: _))

  def intsWithSequence2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))

  def intsWithSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (x, r) = f(rng)
    g(x)(r)
  }

  def nonNegativeIntWithFlatMap: Rand[Int] =
    flatMap(nonNegativeInt) { x =>
      if (x != Int.MinValue) unit(x)
      else nonNegativeIntWithFlatMap
    }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  def map2WithFlatMap2[A, B, C](s1: Rand[A], s2: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(s1) { x1 =>
      flatMap(s2) { x2 =>
        unit(f(x1, x2))
      }
    }

  def map2WithFlatMap[A, B, C](s1: Rand[A], s2: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(s1) { x1 =>
      map(s2)(x2 => f(x1, x2))
    }

  def genericMap[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) =
    s => {
      val (a2, s2) = a(s)
      (f(a2), s2)
    }
}

case class State[S, +A](run: S => (A, S)) {
  import State._

  def mapV2[B](f: A => B): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      (f(a), s2)
    }

  def map2V2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State { s =>
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb.map(b => f(a, b))
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.foldRight(unit[S, List[A]](List.empty[A]))((s2, acc) => s2.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State { s =>
      val state = inputs.foldLeft(s) {
        case (m @ Machine(_, 0, _), _) => m
        case (Machine(true, candies, coins), Coin) => Machine(false, candies, coins + 1)
        case (Machine(false, candies, coins), Turn) => Machine(true, candies - 1, coins)
        case (m @ Machine(_, _, _), _) => m
      }
      ((state.candies, state.coins), state)
    }
  }
}
