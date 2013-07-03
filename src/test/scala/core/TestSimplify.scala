import prime.core._
import prime.core.implicits._
import prime.core.Simplify._

import org.scalatest.FunSuite

class Simplify extends FunSuite {
  val x = Symbol("x")
  val y = Symbol("y")
  val z = Symbol("z")
  test("identities of expand"){
    assert(expand(x) === x)

    assert(expand(x+y) === (x+y))
  }
  test("expand of Multiply"){
    assert(expand((x+y)*(x+y)) === (x**2 + 2*x*y + y**2))

    assert(expand((x+y)*(x+y)*(x+y)) === (x*x*x + 3*y*y*x + 3*x*x*y + y*y*y))

    // fails without reduce on both sides And wierdly Why is there a "- 0" in
    // both sides when reduce is not present on either sides
    // ((3*x*y**2) + x**3 + y**3 + (3*y*x**2) - 0) did not equal
    // (x**3 + y**3 + (3*y*x**2) + (3*x*y**2) - 0)
    assert(expand((x+y)*(x+y)*(x+y)).reduce === (x**3 + 3*y*(x**2) + 3*x*(y**2) + y**3).reduce)

    assert(expand((x+y)*y*z) === (x*y*z + y*y*z))

    assert(expand(x*y).reduce === (x*y))
  }

}
