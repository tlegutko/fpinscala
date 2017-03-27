package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r)
    }
  }

  def sizeWithFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(_ + _)
   }

  def max(t: Tree[Int]): Int = {
    t match {
      case Leaf(x) => x
      case Branch(l, r) => max(l) max max(r)
    }
  }

  def maxWithFold(t: Tree[Int]): Int = {
    fold(t)(x => x)(_ max _)
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def depthWithFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)((l, r) => 1 + (l max r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }
  
  def mapWithFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(x) => f(x)
      case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }
}
