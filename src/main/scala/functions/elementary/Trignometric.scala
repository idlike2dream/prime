package prime.functions.trignometric

import prime.core._
import prime.functions._

case class Sin(arg1: Term) extends ElemementaryFunction with Univariate {

  def funcApply(t: Term) = Sin(t)

  def diff(x: Symbol) = Cos(arg1) * arg1.diff(x)

  def formatToString: String = "sin("+ arg1.reduce +")"

  override def toString = formatToString
}

case class Cos(arg1: Term) extends ElemementaryFunction with Univariate {

  def funcApply(t: Term) = Cos(t)

  def diff(x: Symbol) = -Sin(arg1) * arg1.diff(x)

  def formatToString: String = "sin("+ arg1.reduce +")"

  override def toString = formatToString
}

case class Tan(arg1: Term) extends ElemementaryFunction with Univariate {

  def funcApply(t: Term) = Tan(t)

  def diff(x: Symbol) = (Sec(arg1)**Integer(2)) * arg1.diff(x)

  def formatToString: String = "tan("+ arg1.reduce +")"

  override def toString: String = formatToString
}

case class Csc(arg1: Term) extends ElemementaryFunction with Univariate {

  def funcApply(t: Term) = Csc(t)

  def diff(x: Symbol) = -(Csc(arg1) * Cot(arg1)) * arg1.diff(x)

  def formatToString: String = "csc("+ arg1.reduce +")"

  override def toString: String = formatToString
}

case class Sec(arg1: Term) extends ElemementaryFunction with Univariate {

  def funcApply(t: Term) = Sec(t)

  def diff(x: Symbol) = Tan(arg1) * Sec(arg1) * arg1.diff(x)

  def formatToString: String = "sec("+ arg1.reduce +")"

  override def toString: String = formatToString
}

case class Cot(arg1: Term) extends ElemementaryFunction with Univariate {

  def funcApply(t: Term) = Cot(t)

  def diff(x: Symbol) = -(Csc(arg1)**Integer(2)) * arg1.diff(x)

  def formatToString: String = "cot("+ arg1.reduce +")"

  override def toString: String = formatToString
}
