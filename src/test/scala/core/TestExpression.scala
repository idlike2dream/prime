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
  test("19"){
    assert((1/x).diff(x) === (-1/(x**2)))
  }
  test("20"){
    assert((y/x).diff(x) === (-y/(x**2)))
    //assert((x/x).diff(x) === Integer(0))
    //assert((y/x).diff(x) === y*(-1/(x**2)))
  }
}
