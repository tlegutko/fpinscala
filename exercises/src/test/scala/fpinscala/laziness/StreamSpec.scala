
package fpinscala.laziness

import org.scalatest._

class StreamSpec extends FunSuite {
  test("Stream to List") {
    val stream = Cons(() => "a", () => Cons(() => "b", () => Empty))
    assert(stream.toList() == List("a", "b"))
  }
}
