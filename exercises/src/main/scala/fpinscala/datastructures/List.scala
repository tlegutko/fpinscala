package fpinscala.datastructures

import scala.annotation.tailrec

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

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def product3(ns: List[Double]): Double = {
    def foldR(as: List[Double]): Double = {
      println(as)
      as match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) =>
          val f = foldR(xs)
          if (f == 0) f else x + f
      }
    }
    foldR(ns)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil => Cons(h, Nil)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(List.tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    case Nil => Nil
  }

  def init[A](l: List[A]): List[A] = {
    def reverse(li: List[A]): List[A] = {
      @tailrec
      def go(curr: List[A], toVisit: List[A]): List[A] = toVisit match {
        case Cons(h, t) => go(Cons(h, curr), t)
        case Nil => curr
      }
      go(Nil, li)
    }
    reverse(List.tail(reverse(l)))
  }

  def init2[A](l: List[A]): List[A] = {
    // non tail-rec with just one iteration
    def go(curr: List[A], toVisit: List[A]): List[A] = toVisit match {
      case Nil => Nil
      case Cons(_, Nil) => curr
      case Cons(h, t) => Cons(h, go(curr, t))
    }
    go(Nil, l)
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, cnt: Int) => cnt + 1)
  }

  def length2[A](l: List[A]): Int = {
    @tailrec
    def go(li: List[A], cnt: Int): Int = li match {
      case Cons(_, t) => go(t, cnt + 1)
      case Nil => cnt
    }
    go(l, 0)
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(li: List[A], total: B): B =
      li match {
        case Nil => z
        case Cons(h, Nil) => f(total, h)
        case Cons(h, t) => go(t, f(total, h))
      }
    go(l, z)
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(l: List[Int]): Int =
    List.foldLeft(l, 0)(_ + _)

  def product4(l: List[Double]): Double =
    List.foldLeft(l, 1.0)(_ * _)

  def length3[A](l: List[A]): Double =
    List.foldLeft(l, 0.0)((acc, _) => acc + 1)

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    def go(curr: List[B], toVisit: List[A]): List[B] = toVisit match {
      case Nil => curr
      case Cons(h, t) => Cons(f(h), go(curr, t))
    }
    go(Nil, l)
  }

  def reverse[A](l: List[A]): List[A] = {
    List.foldLeft(l, List[A]())((acc: List[A], curr: A) => Cons(curr, acc))
  }

  def foldLeftWithFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B) = {
    List.foldRight(List.reverse(l), z)((a,b) => f(b,a))
  }

  def foldRightWithFoldLeft[A,B](l: List[A], z: B)(f: (B,A) => B) = {
    List.foldLeft(List.reverse(l), z)(f)
  }

  def appendWithFold[A](l1: List[A], l2: List[A]): List[A] = {
    List.foldRight(l1, l2)(Cons(_, _))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    List.foldRight(l, Nil: List[A])(append)
  }

  def addOne(l: List[Int]): List[Int] = {
    List.map(l)(_ + 1)
  }

  def mapToStr(l: List[Double]): List[String] = {
    List.map(l)(_.toString)
  }

  def mapWithFold[A, B](l: List[A])(f: A => B): List[B] = {
    List.foldRight(l, Nil: List[B]){case (x, acc) => Cons(f(x), acc)}
  }

  def filter[A](l: List[A])(f: A => Boolean) = {
    List.foldRight(l, Nil: List[A])((x, acc) => {
                                      if (f(x)) Cons(x, acc)
                                      else acc
                                    })
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    ???
  }
}
