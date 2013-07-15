package prime.core

case class Symbol(name: String) extends AtomicTerm {
  override def toString = name

  override def diff(x: Symbol): Number =
    if (this == x) Integer(1) else Integer(0)

}

class Constant(val name: String, val value: BigDecimal) extends AtomicTerm {

  override def toString = name

  override def diff(x: Symbol): Number = Integer(0)

}

object Constants {
  object ConstantPi extends Constant("pi", BigDecimal(3.141592653589793)) {
    override def toString = "pi"
  }

  object ConstantE extends Constant("e", BigDecimal(2.718281828459045)) {
    override def toString = "e"
  }

  val pi = ConstantPi

  val e = ConstantE
}
