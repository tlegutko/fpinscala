package fpinscala.datastructures

import org.scalatest._

class ListSpec extends FunSuite {
  test("patternMatching") {
    val x = List(1, 2, 3, 4, 5) match {
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
    assert(List.setHead(l, 2) == List(2, 2))
    assert(List.setHead(l2, 2) == List(2))
  }

  test("init") {
    val l = List(1, 2, 3, 4)
    assert(List.init(l) == List(1, 2, 3))
    assert(List.init2(l) == List(1, 2, 3))
  }

  test("length") {
    val l = List(1, 2, 3)
    assert(List.length2(l) == 3)
    assert(List.length(l) == 3)
  }

  test("product") {
    val l = List(1.0, 2.0, 0, 3.0, 4.0)
    assert(List.product3(l) == 0)
  }

  test("foldRight") {
    assertResult(List(1, 2, 3)) {
      List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    }
  }

  test("foldLeft") {
    val l = List(2, 3, 4)
    assert(List.foldLeft2(l, 1)(_ * _) == 24)
    assert(List.foldLeft(l, 1)(_ * _) == 24)
  }

  test("map") {
    val l = List(1, 2)
    assert(List.map(l)(_.toDouble) == List(1.0, 2.0))
  }

  test("sum, product, length with foldLeft") {
    val l = List(1, 2, 3, 4)
    assert(List.sum3(l) == 10)
    assert(List.length3(l) == 4)
    assert(List.product4(List.map(l)(_.toDouble)) == 24)
  }

  test("reverse") {
    val l = List(1, 2, 3)
    assert(List.reverse(l) == List(3, 2, 1))
  }

  test("foldLeft with foldRight and vice versa") {
    val l = List(1, 2, 3)
    def visitingOrder(f: (List[Int], List[Int]) => ((List[Int], Int) => List[Int]) => List[Int]): List[Int] = {
      List.reverse(f(l, Nil: List[Int])((acc, curr) => Cons(curr, acc)))
    }
    assert(visitingOrder(List.foldLeftWithFoldRight) == l)
    assert(visitingOrder(List.foldRightWithFoldLeft) == List.reverse(l))
  }

  test("append with fold") {
    assert(List.appendWithFold(List(1, 2), List(3)) == List(1, 2, 3))
  }

  test("concat lists with fold") {
    val l = List(List(1, 2), List(3, 4), List(5))
    assert(List.concat(l) == List(1, 2, 3, 4, 5))
  }

  test("map with fold") {
    assert(List.mapWithFold(List(1, 2, 3))(_ + 1) == List(2, 3, 4))
  }

  test("filter") {
    assert(List.filter(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4))
  }

  test("flatMap") {
    assert(List.flatMap(List(1,2))(x => List(x, 3*x)) == List(1, 3, 2, 6))
  }

  test("filter with flatMap") {
    assert(List.filterWithFlatMap(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4))
  }

  test("addTwoLists") {
    assert(List.addTwoLists(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
    assert(List.addTwoLists2(List(1, 2, 3), List(4, 5, 6))(_ + _) == List(5, 7, 9))
  }

  test("hasSubsequence") {
    assert(List.hasSubsequence(List(1, 2, 2, 3, 4), List(2, 3)) == true)
  }

}
