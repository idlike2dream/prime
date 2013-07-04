package prime.core

import scala.math.{BigDecimal => BigDec}

// "I feel the following arrangement is more idiomatic in scala, and will make for better code in the long run:
// Have case classes (val is automatic):
//    case class Integer(value: BigInt) extends ...
//    case class Rational(numer: BigInt, denom: BigInt) extends ...
//    case class Real(value: BigDec) extends ...
// And in code, for example for addition
// def +(that: Number): Number = (this, that) match {
//		case (Rational(a, b), Rational(c,d)) => Rational(ad + bc, bd)
// etc.

sealed trait Number extends AtomicTerm {

  private def toBigDec(x: BigInt) = BigDec(x.toString)

  /** Adds two numbers */
  def + (that: Number): Number = (this, that) match {
    case (a: Rational, b: Rational) => Rational((a.arg1*b.arg2)+(a.arg2*b.arg1), a.arg2*b.arg2)
    case (a: Real, b: Real) => Real(a.arg1 + b.arg1)
    case (a: Integer, b: Integer) => Integer(a.arg1 + b.arg1)
    case (a: Real, b: Rational) => Real((a.arg1*toBigDec(b.arg2) + toBigDec(b.arg1))/toBigDec(b.arg2))
    case (a: Rational, b: Real) => Real((b.arg1*toBigDec(a.arg2) + toBigDec(a.arg1))/toBigDec(a.arg2))
    case (a: Rational, b: Integer) => Rational(a.arg1 + a.arg2*b.arg1, a.arg2)
    case (a: Integer, b: Rational) => Rational(a.arg1*b.arg2 + b.arg1, b.arg2)
    case (a: Real, b: Integer) => Real(a.arg1 + toBigDec(b.arg1))
    case (a: Integer, b: Real) => Real(toBigDec(a.arg1) + b.arg1)
  }

  /** Multiply a number with -1 */
  override def unary_- : Number = this match {
    case x: Rational => Rational(-x.arg1, x.arg2)
    case x: Real => Real(-x.arg1)
    case x: Integer => Integer(-x.arg1)
  }

  /** Subtract two number */
  def - (that: Number): Number = this + (-that)

  /** If number is positive returns 1
    *
    * If number is negative returns -1
    *
    * If number is zero returns 0
    */
  def signum: Int = this match {
    case x: Rational => x.arg1.signum*x.arg2.signum
    case x: Real => x.arg1.signum
    case x: Integer => x.arg1.signum
  }

  /** Multiply two numbers */
  def * (that: Number): Number = (this, that) match {
    case (a: Rational, b: Rational) => Rational(a.arg1*b.arg1, a.arg2*b.arg2)
    case (a: Real, b: Real) => Real(a.arg1*b.arg1)
    case (a: Integer, b: Integer) => Integer(a.arg1*b.arg1)
    case (a: Real, b: Rational) => Real(a.arg1*toBigDec(b.arg1)/toBigDec(b.arg2))
    case (a: Rational, b: Real) => Real(toBigDec(a.arg1)*b.arg1/toBigDec(a.arg2))
    case (a: Rational, b: Integer) => Rational(a.arg1*b.arg1, a.arg2)
    case (a: Integer, b: Rational) => Rational(a.arg1*b.arg1, b.arg2)
    case (a: Real, b: Integer) => Real(a.arg1*toBigDec(b.arg1))
    case (a: Integer, b: Real) => Real(toBigDec(a.arg1)*b.arg1)
  }

  /** Divide two numbers */
  def / (that: Number): Number = (this, that) match {
    case (a: Rational, b: Rational) => Rational(a.arg1*b.arg2, a.arg2*b.arg1)
    case (a: Real, b: Real) => Real(a.arg1 / b.arg1)
    case (a: Integer, b: Integer) => Rational(a.arg1, b.arg1)
    case (a: Real, b: Rational) => Real(a.arg1/toBigDec(b.arg1)*toBigDec(b.arg2))
    case (a: Rational, b: Real) => Real(toBigDec(a.arg1)/b.arg1*toBigDec(a.arg2))
    case (a: Rational, b: Integer) => Rational(a.arg1, a.arg2*b.arg1)
    case (a: Integer, b: Rational) => Rational(a.arg1*b.arg2, b.arg1)
    case (a: Real, b: Integer) => Real(a.arg1 / toBigDec(b.arg1))
    case (a: Integer, b: Real) => Real(toBigDec(a.arg1)/b.arg1)
  }

  /** Absolute value of a number*/
  override def abs : Number = this match {
    case x: Integer => Integer(x.signum*x.arg1)
    case x: Real => Real(x.signum*x.arg1)
    case x: Rational => Rational(x.signum*x.arg1, x.arg2)
  }

  /** Differentiation of a number always returns 0 */
  def diff(x: Symbol): Number = Integer(0)

  // #TODO Need to write ** (power) method
}

class Integer(val arg1: BigInt) extends Number with Ordered[Integer] {

  override def toString = arg1.toString

  override def equals(that: Any): Boolean = that match {
    case that: Rational => (arg1==that.arg1) && (BigInt(1) == that.arg2)
    case that: Real => (arg1 == that.arg1.toBigIntExact)
    case that: Integer => (arg1 == that.arg1)
    case _ => false
  }

  override def hashCode = toString.hashCode

  def compare(that: Integer) = (this - that).signum

}

object Integer {
  def apply(a: Int) = new Integer(BigInt(a))
  def apply(a: Long) = new Integer(BigInt(a))
  def apply(a: String) = new Integer(BigInt(a))
  def apply(a: BigInt) = new Integer(a)
}

class Rational(val arg1: BigInt, val arg2: BigInt = BigInt(1)) extends Number with Ordered[Rational] {
  require(arg2!=BigInt(0), throw new IllegalArgumentException)
  override def toString =
    if (arg2.toString == "1") arg1.toString
    else arg1 + "/" + arg2

  // #TODO Write case for even Rational(1, 4) == Real(0.25) to be true
  // May be write Real to Rational method Since precision is finite any way.

  override def equals(that: Any): Boolean = that match {
    case that: Rational => (arg1==that.arg1) && (arg2 == that.arg2)
    case that: Real => (arg1 == that.arg1.toBigIntExact) && (arg2== BigInt(1))
    case that: Integer => (arg1 == that.arg1) && (arg2== BigInt(1))
    case _ => false
  }

  override def hashCode = toString.hashCode

  def compare(that: Rational) = (that - this).signum
}

object Rational {

  def apply(a: BigInt, b: BigInt) = {
    val g = a.gcd(b);
    if ((a>0 && b>0) || (a< 0 && b>0)) new Rational(a/g, b/g)
    else new Rational(-a/g, -b/g)
  }
  def apply(a: BigInt) = new Rational(a)

  def apply(a: Int, b: Int): Rational = Rational(BigInt(a), BigInt(b))
  def apply(a: Int): Rational = new Rational(BigInt(a))

  def apply(a: Integer, b: Integer): Rational = Rational(a.arg1, b.arg1)
  def apply(a: Integer): Rational = new Rational(a.arg1)

  def apply(a: Long, b: Long): Rational = Rational(BigInt(a), BigInt(b))
  def apply(a: Long): Rational = new Rational(BigInt(a))

  def apply(a: String): Rational = new Rational(BigInt(a))
  def apply(a: String, b: String): Rational = Rational(BigInt(a), BigInt(b))

  def apply(a: Int, b: Rational): Rational = Rational(apply(a).arg1 * b.arg2, b.arg1)
  def apply(a: Rational, b: Int): Rational = Rational(a.arg1, apply(b).arg1 * a.arg2)

  def apply(a: Integer, b: Rational): Rational = Rational(a.arg1 * b.arg2, b.arg1)
  def apply(a: Rational, b: Integer): Rational = Rational(a.arg1, b.arg1 * a.arg2)

}

class Real(val arg1: BigDec) extends Number with Ordered[Real]{

  override def equals(that: Any): Boolean = that match {
    case that: Real => (arg1==that.arg1)
    case that: Rational => (that.arg1 == arg1.toBigIntExact) && (that.arg2== BigInt(1))
    case that: Integer => (arg1.toBigIntExact == that.arg1)
    case _ => false
  }

  // bigDecimal is val member of BigDecimal of type java.math.BigDecimal
  // This is a ugly way to stripTrailingZeros
  override def hashCode = arg1.bigDecimal.stripTrailingZeros.toString.hashCode

  override def toString = arg1.toString

  def compare(that: Real) = (this - that).signum
}

object Real {

  def apply(a: Double) = new Real(BigDec(a))
  def apply(a: Float) = new Real(BigDec(a))
  def apply(a: String) = new Real(BigDec(a))
  def apply(a: BigDec) = new Real(a)
}
