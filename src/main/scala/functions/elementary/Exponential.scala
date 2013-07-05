package prime.functions.elementary

import prime.core._
import prime.functions._

case class Exp(arg1: Term) extends ElemementaryFunction with Univariate {

  def funcApply(t: Term) = Exp(t)

  def diff(x: Symbol) = this * arg1.diff(x)

  def formatToString: String = "exp("+ arg1.reduce +")"

  override def toString: String = formatToString
}

case class Log(arg1: Term) extends ElemementaryFunction with Univariate {

  def funcApply(t: Term) = Log(t)

  def diff(x: Symbol) = (Integer(1)/arg1) * arg1.diff(x)

  def formatToString: String = "log("+ arg1.reduce +")"

  override def toString: String = formatToString
}
