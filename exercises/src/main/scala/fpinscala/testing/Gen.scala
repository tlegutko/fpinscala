package fpinscala.testing

import fpinscala.laziness.{ Cons, Stream }
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ Executors, ExecutorService }

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (maxSize, testCases, rng) =>
      run(maxSize, testCases, rng) match {
        case Right((s1, n1)) => p.run(maxSize, testCases, rng).map {
          case (s2, n2) => (s1, s2) match {
            case (Proven, Proven) => (Proven, n1 + n2)
            case _ => (Unfalsified, n1 + n2)
          }
        }
        case l => l
      }
  }

  def ||(p: Prop): Prop =
    Prop { (maxSize, testCases, rng) =>
      run(maxSize, testCases, rng) match {
        case Left(msg1) => p.run(maxSize, testCases, rng).left.map(msg2 => msg1 + "\n" + msg2)
        case r => r
      }
    }

  def check: Either[FailedCase, (Status, SuccessCount)] = ???
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type Result = Either[FailedCase, (Status, SuccessCount)]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      {
        def go(i: Int, j: Int, s: Stream[Option[A]], onEnd: Int => Result): Result =
          if (i == j) Right((Unfalsified, i))
          else s match {
            case Cons(h, t) => h() match {
              case Some(h) =>
                try {
                  if (f(h)) go(i + 1, j, t(), onEnd)
                  else Left(h.toString)
                } catch { case e: Exception => Left(buildMsg(h, e)) }
              case None => Right((Unfalsified, i))
            }
            case _ => onEnd(i)
          }
        go(0, n / 3, a.exhaustive, i => Right((Proven, i))) match {
          case Right((Unfalsified, _)) =>
            val rands = randomStream(a)(rng).map(Some(_))
            go(n / 3, n, rands, i => Right((Unfalsified, i)))
          case s => s // If proven or failed, stop immediately
        }
      }
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = n / max + 1
      val props: List[Prop] =
        Stream.from(0).take(max + 1).map(i => forAll(g)(f)).toList
      val p: Prop = props.map(p => Prop((max, n, rng) => p.run(max, casesPerSize, rng))).
        reduceLeft(_ && _)
      p.run(max, n, rng).map {
        case (Proven, n2) => (Exhausted, n2)
        case p => p
      }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = g match {
    case Unsized(g2) => forAll(g2)(f)
    case Sized(g3) => forAll(g3)(f)
  }

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")

}

trait Status

case object Exhausted extends Status
case object Proven extends Status
case object Unfalsified extends Status

object Gen {
  type Domain[+A] = Stream[Option[A]]

  def bounded[A](a: Stream[A]): Domain[A] = a.map(Some(_))

  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), bounded(Stream(a)))

  // ex. 4, no longer compiles
  // type Gen[A] = State[RNG, A]
  // def choose(start: Int, stopExclusive: Int): Gen[Int] = {
  //   val range = stopExclusive - start
  //   State(rng => RNG.positiveMax(range)(rng)).map(_ + start)
  // }

  def boolean: Gen[Boolean] = {
    def bool(rng: RNG): (Boolean, RNG) =
      RNG.map(RNG.int)(_ % 2 == 0)(rng)
    Gen(State(bool), bounded(Stream(true, false)))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)),
      bounded(Stream.from(start).take(stopExclusive - start)))
  }

  def listOfN2[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    def exhaustiveListStream(s: Stream[Option[A]]): Stream[Option[List[A]]] = {
      def sequenceOption(o: List[Option[A]]): Option[List[A]] = {
        o.foldRight[Option[List[A]]](Some(List.empty[A])) {
          (h, t) => for { a <- h; b <- t } yield a :: b
        }
      }
      val ss = Stream.constant(s).take(n)
      val cartesian = ss.foldRight(Stream(Stream[Option[A]]())) {
        case (h, t) => for { a <- h; b <- t } yield Stream.cons(a, b)
      }
      cartesian.map(s => sequenceOption(s.toList))
    }
    Gen(State.sequence(List.fill(n)(g.sample)), exhaustiveListStream(g.exhaustive))
  }

  def uniform: Gen[Double] =
    Gen(State(RNG.double), unbounded)

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(State(RNG.double).map(d => i + d * (j - i)), unbounded)

  def chooseViaUniformAndMap(i: Double, j: Double): Gen[Double] =
    uniform.map(d => i + d * (j - i))

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = {
    def even: Gen[Int] = choose(from, if (to % 2 == 0) to - 1 else to)
      .map(i => if (i % 2 == 0) i else i + 1)
    def odd: Gen[Int] = choose(from, if (to % 2 != 0) to - 1 else to)
      .map(i => if (i % 2 != 0) i else i + 1)
    for {
      a <- choose(from, to)
      b <- if (a % 2 == 0) even else odd
    } yield (a, b)
    // same as:
    // choose(from, to).flatMap{ a =>
    //   val a2 = if (a % 2 == 0) even else odd
    //   a2.map(b => (a, b))
    // }
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(unit(List.empty[A]))((a, b) => a.map2(b)(_ :: _))

  def union2[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    def interleave(s1: Stream[Option[A]], s2: Stream[Option[A]]): Stream[Option[A]] =
      s1.zipAll(s2).flatMap {
        case (a, b) => Stream((a.toList ++ b.toList): _*)
      }
    Gen(State(RNG.boolean).flatMap(b => if (b) g1.sample else g2.sample),
      interleave(g1.exhaustive, g2.exhaustive))
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Limit = g1._2 / (g1._2 + g2._2)
    def interleave(s0: Stream[Double], s1: Stream[Option[A]], s2: Stream[Option[A]]): Stream[Option[A]] = s0 match {
      case Cons(h0, t0) if (h0() < g1Limit) =>
        s1 match {
          case Cons(h1, t1) => Stream.cons(h1(), interleave(t0(), t1(), s2))
          case _ => s2
        }
      case Cons(h0, t0) =>
        s2 match {
          case Cons(h2, t2) => Stream.cons(h2(), interleave(t0(), s1, t2()))
          case _ => s1
        }
      case _ => Stream.empty
    }
    def randStream: Stream[Double] =
      Stream.unfold(RNG.Simple(System.currentTimeMillis()): RNG)(rng => Some(RNG.double(rng)))
    Gen(State(RNG.double).flatMap(d => if (d < g1Limit) g1._1.sample else g2._1.sample), interleave(randStream, g1._1.exhaustive, g2._1.exhaustive))
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    Sized { n => Gen.listOfN(n, g) }

}

case class Gen[+A](sample: State[RNG, A], exhaustive: Stream[Option[A]]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f), exhaustive.map(_.map(f)))

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(b.sample)(f), exhaustive.map2(b.exhaustive) {
      (oa, ob) => for { a <- oa; b <- ob } yield f(a, b)
    })

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample), exhaustive.flatMap {
      case None => unbounded
      case Some(b) => f(b).exhaustive
    })

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN2(n, this))

  def unsized: SGen[A] =
    Unsized(this)
}

case class Sized[+A](forSize: Int => Gen[A]) extends SGen[A]
case class Unsized[+A](get: Gen[A]) extends SGen[A]

trait SGen[+A] {

  def map[B](f: A => B): SGen[B] = this match {
    case Sized(fs) => Sized(fs(_).map(f)) // or Sized(fs andThen (_ map f))
    case Unsized(g) => Unsized(g.map(f))
  }

  def flatMap[B](f: A => Gen[B]): SGen[B] = this match {
    case Sized(fs) => Sized(fs(_).flatMap(f)) // or Sized(fs andThen (_ flatMap f))
    case Unsized(g) => Unsized(g.flatMap(f))
  }
}

