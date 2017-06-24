package fpinscala.laziness

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer
import scala.Option
import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList1(): List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList1()
  }

  def toList2(): List[A] = {
    @annotation.tailrec
    def go(stream: Stream[A], list: List[A]): List[A] = stream match {
      case Cons(h, t) => go(t(), h() :: list)
      case Empty => list
    }
    go(this, List.empty).reverse
  }

  def toList(): List[A] = {
    @annotation.tailrec
    def go(stream: Stream[A], list: ListBuffer[A]): List[A] = stream match {
      case Cons(h, t) => go(t(), list += h())
      case Empty => list.toList
    }
    go(this, new ListBuffer())
  }

  def take2(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: Stream[A], cnt: Int): Stream[A] = cnt match {
      case x if (x < n) => s match {
        case Cons(h, t) => go(t(), Cons(h, () => acc), cnt + 1)
        case Empty => acc
      }
      case _ => acc
    }
    go(this, Empty, 0).reverse
  }

  def reverse(): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: Stream[A]): Stream[A] = s match {
      case Cons(h, t) => go(t(), Cons(h, () => acc))
      case Empty => acc
    }
    go(this, Empty)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Cons(h, () => t().take(n - 1))
    case Cons(h, _) if n == 1 => Cons(h, () => Empty)
    case Empty => Empty
  }

  def drop2(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], cnt: Int): Stream[A] = s match {
      case Cons(h, t) if cnt < n => go(t(), cnt + 1)
      case Cons(h, _) if cnt == n => s
      case Empty => Empty
    }
    go(this, 0)
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n >= 1 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhileWithFold(p: A => Boolean): Stream[A] = {
    foldRight(empty: Stream[A])((a, b) => p(a) match {
      case true => cons(a, b)
      case false => empty
    })
  }

  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case _ => None
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](x: => Stream[B]): Stream[B] =
    foldRight(x)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def mapWithUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeWithUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), x) if x > 0 => Some((h(), (t(), x - 1)))
      case _ => None
    }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), empty) => Some(((Some(h1()), None), (t1(), empty)))
      case (empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean = {
    zip(s).forAll {
      case (a, b) if a == b => true
      case _ => false
    }
  }

  def tails(): Stream[Stream[A]] =
    unfold(this) {
      case s @ Cons(h, t) => Some((s, t()))
      case _ => None
    } append Stream(empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight(z, Stream(z)){
      case (h, (x, s)) => {
        val fhx = f(h, x)
        (fhx, cons(fhx, s))
      }
    }._2
  }

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
<<<<<<< HEAD
  def from(n: Int): Stream[Int] = ???
=======

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(curr: Int, prev: Int): Stream[Int] = {
      cons(prev, go(curr + prev, curr))
    }
    go(1, 0)
  }

<<<<<<< HEAD
>>>>>>> Done up to ex. 9 in laziness chapter.
=======
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  def constantWithUnfold[A](a: A): Stream[A] =
    unfold(a)(a => Some((a, a)))

  def fromWithUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))
>>>>>>> Unfold (ex. 10-11 from laziness).

  def fibsWithUnfold(): Stream[Int] =
    unfold(1, 0) { case (curr, prev) => Some((prev, (curr + prev, curr))) }

}
