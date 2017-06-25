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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
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

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    ((-n / Int.MinValue).toDouble, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
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
      }
      else (acc.toList, r)
    }
    go(count, rng, ListBuffer.empty)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(c: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (c > 0) {
        val (n, r2) = r.nextInt
        go(c - 1, r2, n :: acc)
      }
      else (acc.toList, r)
    }
    go(count, rng, List.empty)
  }

  def positiveMax(n: Int): Rand[Int] = 
    map(rng => rng.nextInt)(_ % (n+1))

  def doubleWithMap(): Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

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
