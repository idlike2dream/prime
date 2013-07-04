import prime.core._
import prime.core.implicits._

import org.scalatest.FunSuite

class Number extends FunSuite {
  val x = Symbol("x")

  test("Integer"){                  //OK
    assert(Integer("22")===Integer(22))
    assert(Integer("3455555555".toLong) === Integer("3455555555"))
    assert(Integer(BigInt("45"*35))===Integer("45"*35))
    assert(Integer(2)+Integer(3) === Integer(5))
    assert(2+Integer(3) === Integer(5))
    assert(Integer(5)-3===Integer(2))
    assert(Integer(2)*Integer(3) === Integer(6))
    assert(2*Integer(3)===Integer(6))
    assert(4/Integer(6)===Rational(2, 3))
    assert(-Integer(3)===Integer(-3))
    assert(-Integer(0)===Integer(0))
    assert(6.diff(x)===Integer(0))
    assert(Integer("34555"*34).diff(x, 23)===Integer(0))
    assert(Integer(6).signum === 1)
    assert(Integer(6).signum === 1)
    assert(Integer(-6).signum === -1)
//    assert(6.signum === 1)
//    both method intWrapper in class LowPriorityImplicits of type (x: Int)scala.runtime.RichInt
//    and method intToNumber in object implicits of type (x: Int)prime.core.Integer
//    are possible conversion functions from Int(6) to ?{def signum: ?}
    assert(Integer(6).abs === Integer(6))
    assert(Integer(-3).abs === Integer(3))
//    assert(6.abs ===6) fails for same above reason
  }
  test("Rational"){                 //OK
    assert(Rational("4", "6") === Rational(2, 3))
    assert(Rational("2")===Rational(2))
    assert(Rational(-1, -1)===Rational(1))
    assert(Rational(2, -3)===Rational(-2, 3))
    assert(Rational("3".toLong)===Rational(3))
    assert(Rational("6".toLong, "4".toLong)===Rational(3, 2))
    assert(Rational(BigInt(3), BigInt(6))===Rational(1, 2))
    assert(Rational(Integer(3), Integer(6))===Rational(1, 2))
    assert(Rational(Integer(7))===Rational(14, 2))
    assert(Rational(Rational(2, 6), Integer(3))===Rational(1, 9))
    assert(Rational(Integer(20), Rational(35, 30))===Rational(120, 7))
    assert(Integer(2)===Rational(2))
    assert(Rational(2, 3)+Rational(1, 6)===Rational(5, 6))
    assert(Rational(5, 10)+ 4 === Rational(9, 2))
    assert(Rational(6, 24)+ Integer(6) === Rational(25, 4))
    assert(4 + Rational(5, 10) === Rational(9, 2))
    assert(Integer(6) + Rational(6, 24) === Rational(25, 4))
    assert(Rational(2, 3)-Rational(1, 6) === Rational(1, 2))
    assert(Rational(5, 10)- 4 === Rational(-7, 2))
    assert(Rational(6, 24)- Integer(6) === Rational(-23, 4))
    assert(4 - Rational(5, 10) === Rational(7, 2))
    assert(Integer(6) - Rational(6, 24) === Rational(23, 4))
    assert(Rational(2, 3)*Rational(1, 6) === Rational(1, 9))
    assert(Rational(5, 10)* 4 === Rational(2))
    assert(Rational(6, 24)*Integer(6) === Rational(3, 2))
    assert(4 * Rational(5, 10) === Rational(2))
    assert(Integer(6) * Rational(6, 24) === Rational(3, 2))
    assert(Rational(2, 3)*Rational(1, 6) === Rational(1, 9))
    assert(Rational(5, 10)/4 === Rational(1, 8))
    assert(Rational(6, 24)/Integer(6) === Rational(1, 24))
    assert(4 / Rational(5, 10) === Rational(8))
    assert(Integer(6) / Rational(6, 24) === Rational(24))
    assert(Rational(5, 10).signum === 1)
    assert(Rational(-6, 24).signum === -1)
    assert(Rational(5, 10).diff(x) === Integer(0))
    assert(Rational(6, 24).diff(x, 34) === Integer(0))
  }
  test("Real"){

  }
}
