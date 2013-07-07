import prime.core._
import prime.functions.elementary._
import prime.core.implicits._

import org.scalatest.FunSuite

class ElementaryFunctions extends FunSuite {
  val x = Symbol("x")
  val y = Symbol("y")

  test("exponential") {
    assert(Exp(x).diff(x).delIdentity.singleTerm === Exp(x))
    assert(Exp(x**2).diff(x).delIdentity === 2*x*Exp(x**2))
    //assert(Exp(-x).diff(x).reduce === -Exp(-x))
    assert((Exp(x)**2).diff(x).flatten.delIdentity === (2*Exp(x)*Exp(x)).flatten)
  }

  test("logaritm"){
    assert(Log(x).diff(x).delIdentity.singleTerm === 1/x)
    //assert(Log(x**2).diff(x).reduce === 2/x)
  }
}
