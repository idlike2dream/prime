import prime.core._
import prime.functions.elementary._
import prime.core.implicits._

import org.scalatest.FunSuite

class ElementaryFunctions extends FunSuite {
  val x = Symbol("x")
  val y = Symbol("y")

  test("exponential") {
    assert(Exp(x).diff(x) === Exp(x))
    assert(Exp(x**2).diff(x) === 2*x*Exp(x**2))
    assert(Exp(-x).diff(x).reduce === -Exp(-x))
    assert((Exp(x)**2).diff(x) === 2*Exp(x)*Exp(x))
  }

  test("logaritm"){
    assert(Log(x).diff(x).reduce === 1/x)
    //assert(Log(x**2).diff(x).reduce === 2/x)
  }
}
