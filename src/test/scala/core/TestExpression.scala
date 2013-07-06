import prime.core._
import prime.core.implicits._

import org.scalatest.FunSuite

// Unless explicitly stated assume that test passes

class Expression extends FunSuite {
  val x = Symbol("x")
  val y = Symbol("y")
  val z = Symbol("z")

  test("1"){
    assert(x.diff(x)===Integer(1))
  }
  test("2"){
    assert((x*1*1*1*1*1)===x)
  }
  test("3"){
    assert((x+1+1+2) === (x+4))
  }
  test("4"){
    assert((x*x*x*x*x).reduce.diff(x) === (5*(x**4)))
  }
  test("5"){
    assert((x+x+x+x).reduce === (4*x))
  }
  test("6"){
    assert((x*x*x*x*x).reduce === (x**5))
  }
  test("7"){
    assert(((x*x*x)+(x*x)+(x)).reduce.diff(x) === (3*(x**2) + 2*x + 1))
  }
  test("8"){
    assert((x*x*x).reduce.diff(x, 3) === Integer(6))
  }
  test("9"){
    assert((x*x*x*x).reduce.diff(x, 4) === Integer(24))
  }
  test("10"){
    assert((x*x*x*(0*y*y*x)) === Integer(0))
  }
  test("11"){
    assert((x-0) === x)
  }
  test("12"){
    assert((x-y).toString === "(x - y)")
  }
  test("13"){
    assert((x-y-y) === (x - 2*y))
  }
  test("14"){
    assert((x*3*4*2*x*x) === (24*(x**3)))
  }
  test("15"){
    assert((-(-x)) === x)
  }
  test("16"){
    assert((x-y-z)=== (x -(y+z)))
  }
  test("17"){
    assert((x*(-y)*(-3)*(-z)).reduce === (1)*(-3)*x*y*z)
  }
  test("18"){
    assert((-1*x).abs === x)
  }
  test("Division Differentiation"){
    assert((1/x).diff(x) === (-1/(x**2)))
    assert((y/x).diff(x) === (-y/(x**2)))
    assert((x/x).diff(x).cancel === Integer(0))
    //assert((y/x).diff(x) === y*(-1/(x**2)))
  }
  test("Cancel"){
    assert((x-x).cancel === Integer(0))
    assert((x-x+y-y).reducePartial.cancel === Integer(0))
    assert((2*x - 2*x).cancel === Integer(0))
    assert((x*(y -y)).cancel === Integer(0))
    assert((x-x+x).reducePartial.cancel === x)
  }
  test("identities of expand"){
    assert((x).expand === x)

    assert((x+y).expand === (x+y))

    assert((x**2).expand.reduce === x**2)

    assert((x*y).expand.reduce === (x*y))
  }
  test("expand of Multiply"){
    assert(((x+y)*(x+y)).expand === (x**2 + 2*x*y + y**2))

    assert(((x+y)*(x+y)*(x+y)).expand === (x*x*x + 3*y*y*x + 3*x*x*y + y*y*y))

    // fails without reduce on both sides And wierdly Why is there a "- 0" in
    // both sides when reduce is not present on either sides
    // ((3*x*y**2) + x**3 + y**3 + (3*y*x**2) - 0) did not equal
    // (x**3 + y**3 + (3*y*x**2) + (3*x*y**2) - 0)
    assert(((x+y)*(x+y)*(x+y)).expand.reduce === (x**3 + 3*y*(x**2) + 3*x*(y**2) + y**3).reduce)

    assert(((x+y)*y*z).expand === (x*y*z + y*y*z))
  }
  test("expand of power"){
    assert(((x+y)**2).expand === (x**2 + 2*x*y + y**2))

    assert(((x+y)**3).expand.reduce === (x**3 + 3*y*(x**2) + 3*x*(y**2) + y**3).reduce)
  }
}
