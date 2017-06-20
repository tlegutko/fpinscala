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

  def map[B](f: A => B): Stream[A] = ???

  def filter(f: A => Boolean): Stream[A] = ???

  def append(x: A): Stream[A] = ???

  def flatMap(f: A => Stream[A]): Stream[A] = ???

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
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
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}
