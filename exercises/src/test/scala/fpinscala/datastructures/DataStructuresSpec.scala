package fpinscala.datastructures

import org.scalatest._

class DataStructuresSpec extends FunSuite {
  test("patternMatching") {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    assert(x == 3)
  }

  test("tail") {
    val l1 = List("a", "b")
    val l2 = List("b")
    val l3 = Nil
    assert(List.tail(l1) == l2)
    assert(List.tail(l2) == l3)
    assert(List.tail(l3) == l3)
  }

  test("drop") {
    val l = List(1, 2, 5, 8)
    assert(List.drop(l, 3) == List(8))
  }

  test("dropWhile") {
    val l = List(1, 2, 3, 4)
    val f = (a: Int) => a < 4
    assert(List.dropWhile(l, f) == List(4))
  }

  test("setHead") {
    val l = List(1, 2)
    val l2 = Nil
    assert(List.setHead(l, 2) == List(2,2))
    assert(List.setHead(l2, 2) == List(2))
  }

  test("init") {
    val l = List(1, 2, 3, 4)
    assert(List.init(l) == List(1,2,3))
  }

  test("length") {
    val l = List(1, 2, 3)
    assert(List.length(l) == 3)
  }
}
