package prime.functions

import prime.core._

case class Exp(arg1: Term) extends ElemementaryFunction with Univariate {

  def funcApply(t: Term) = Exp(t)

  def diff(x: Symbol) = CompositeTerm(BinOp("*"), List(this, arg1.diff(x)))

  def formatToString: String = "exp("+ arg1.reduce +")"

}
