package fpinscala.datastructures

import org.scalatest._

class TreeSpec extends FunSuite {

  test("size") {
    val t1 = Leaf(1)
    val t2 = Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))
    assert(Tree.size(t1) == 1)
    assert(Tree.size(t2) == 3)
    assert(Tree.sizeWithFold(t1) == 1)
    assert(Tree.sizeWithFold(t2) == 3)
  }

  test("max") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    assert(Tree.max(t) == 4)
    assert(Tree.maxWithFold(t) == 4)
  }

  test("depth") {
    val t = Branch(
      Branch(
        Branch(
          Leaf(1), Leaf(1)),
        Leaf(1)),
      Leaf(1))
    assert(Tree.depth(t) == 3)
    assert(Tree.depthWithFold(t) == 3)
  }

  test("map") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val et = Branch(Branch(Leaf("1"), Leaf("2")), Branch(Leaf("3"), Leaf("4")))
    assert(Tree.map(t)(_.toString) == et)
    assert(Tree.mapWithFold(t)(_.toString) == et)
  }

  test("fold") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    assertResult(10){
      Tree.fold(t)(x => x)(_ + _)
    }
  }
}
