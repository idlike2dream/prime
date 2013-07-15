import prime.core._
import prime.core.implicits._

import org.scalatest.FunSuite

// Unless explicitly stated assume that test passes

class Expression extends FunSuite {
  val x = Symbol("x")
  val y = Symbol("y")
  val z = Symbol("z")

  test("flatten") {
    val a = (x+(y+(y+z)))
    val b = (x*y*(x*(y+(z+(x*y*z)))))

    assert((x*(y*z)).flatten === (x*y*z).flatten)
    assert((x/(y+x+z)).flatten === (x/(x+z+y)).flatten)
    assert(a.flatten === (x+y+y+z).flatten)
    assert(b.flatten === (x*y*x*(y + z + (x*y*z))).flatten)
  }
  test("delIdentity") {
    val a = (0+(0+(0+x)))
    val b = (y*1*(1*(0+(0+(1*1*x)))))

    assert(a.flatten.delIdentity === (x+0).delIdentity)
    assert(b.flatten.delIdentity === (y*(0 + (x*1))).flatten.delIdentity)
    assert((x/1).delIdentity === x)
    assert((-(-x)).delIdentity === x)
  }
  test("simplifyTerm") {
    val a = (0+(0+(0+x))).flatten.delIdentity
    val b = (y*1*(1*(0+(0+(1*1*x))))).flatten.delIdentity

    assert(a.simplifyTerm === x)
    assert(b.simplifyTerm === x*y)
    assert((1/(1/x)).simplifyTerm.delIdentity.simplifyTerm === x)
    assert((x/(x/y)).simplifyTerm === (x*y)/x)
    assert(((x/y)/(y/x)).simplifyTerm === (x*x)/(y*y))
  }
  test("mulZero"){
    val a = (x+(y+(y+z)*0))
    val b = (x*y*(x*(y*0+(z+(x*y*z))*0)))

    assert((x*y*x*x*0).flatten.mulZero === Integer(0))
    assert(a.flatten.mulZero.delIdentity === (x+y))
    assert((0/x).mulZero === Integer(0))
    assert(b.flatten.mulZero.delIdentity.simplifyTerm.mulZero === Integer(0))
  }
  test("reduceNumber"){
    assert((x+1+1+1).flatten.reduceNumber.toString === "(x + 3)")
    assert((x*3*4*1).flatten.reduceNumber.toString === "(12*x)")
  }
  test("groupNegative"){
    assert((x*(-y)*(-z)).flatten.groupNegative === (x*y*z).flatten)
  }
  test("groupDivide"){
    assert(((x/y)*(x/z)).flatten.groupDivide.opSimp === (x**2)/(y*z))
  }
  test("reduce"){
    val a1 = (x+(y+(y+z)))
    val a2 = (x*y*(x*(y+(z+(x*y*z)))))

    assert((x*(y*z)).reduce === (x*y*z).reduce)
    assert(a1.reduce === (x+y+y+z).reduce)
    assert(a2.reduce === (x*y*x*(y + z + (x*y*z))).reduce)

    val a3 = (0+(0+(0+x)))
    val a4 = (y*1*(1*(0+(0+(1*1*x)))))

    assert(a3.reduce === (x+0).reduce)
    assert(a4.reduce === (y*(0 + (x*1))).reduce)
    assert((x/1).reduce === x)
    assert((-(-x)).reduce === x)

    val a5 = (0+(0+(0+x))).reduce
    val a6 = (y*1*(1*(0+(0+(1*1*x))))).reduce

    assert(a5 === x)
    assert(a6 === x*y)

    assert((x+x+x+y+y+z).reduce === (3*x + 2*y +z).reduce)
    assert((x*x*x*y*y*z).reduce === ((x**3)*(y**2)*z).reduce)

    val a7 = (x+(y+(y+z)*0))
    val a8 = (x*y*(x*(y*0+(z+(x*y*z))*0)))

    assert((x*y*x*x*0).reduce === Integer(0))
    assert(a7.reduce === (x+y))
    assert((0/x).reduce === Integer(0))
    assert(a8.reduce === Integer(0))

    assert((x+1+1+1).reduce.toString === "(x + 3)")
    assert((x*3*4*1).reduce.toString === "(12*x)")

    assert((x*(-y)*(-z)).flatten.groupNegative === (x*y*z).flatten)

    assert(((x/y)*(x/z)).reduce === (x**2)/(y*z))

    assert((x-x).reduce === Integer(0))
    assert((x-x+y-y).reduce.reduceNumber.groupNegative === Integer(0))
    assert((2*x - 2*x).reduce === Integer(0))
    assert((x*(y -y)).reduce === Integer(0))
    assert((x-x+x).reduce === x)
    assert((x - (x - x + (x + x -x ))).reduce === Integer(0))

    assert((5*x).reduce === 5*x)
    assert((x+5*x).reduce === 6*x)
    assert((x - y + 3*x + 4*y + x - 3*x + x*y).reduce ===  (x*y + 2*x + 3*y).flatten)
    assert((x+y+z).reduce === (x + y + z).flatten)
    assert(x*(x+x+3*x).reduce === 5*x*x)
    assert((x-x).reduce === Integer(0))
    assert((x-x+y-y).reduce === Integer(0))
    assert((-(x+y+z) + z + y).reduce === -x )
    assert((2*x - 2*x).reduce === Integer(0))
    assert((x*(y -y)).reduce === Integer(0))
    assert((x*y + y*x).reduce === (2*x*y).flatten)
    assert((x*y*z + 3*y*x*z).reduce === (4*x*y*z).flatten)
    assert((x-x+x).reduce === x)
    assert((x - (x - x + (x + x -x ))).reduce === Integer(0))
    assert((x**3**2).reduce == x**6)
    assert((x*y*(y**2/z)).reduce === ((x*(y**3))/z).flatten)
    assert(((x/y)*(y/z)*(z/x)).reduce === Integer(1))
    assert(((x**2/y)*(y**2/z)*(z**2/x)).reduce === (x*y*z).flatten)
  }

  test("addsub"){
    assert((5*x).opSimp === 5*x)
    assert((x+5*x).opSimp.simplifyTerm === 6*x)
    assert((x - y + 3*x + 4*y + x - 3*x + x*y).flatten.opSimp ===  (x*y + 2*x + 3*y).flatten)
    assert((x+y+z).flatten.opSimp === (x + y + z).flatten)
    assert(x*(x+x+3*x).flatten.opSimp.simplifyTerm === 5*x*x)
    assert((x-x).opSimp.simplifyTerm === Integer(0))
    assert((x-x+y-y).flatten.opSimp.simplifyTerm === Integer(0))
    assert((-(x+y+z) + z + y).flatten.opSimp.groupNegative.simplifyTerm === -x )
    assert((2*x - 2*x).opSimp.simplifyTerm === Integer(0))
    assert((x*(y -y)).opSimp.simplifyTerm.mulZero === Integer(0))
    assert((x-x+x).flatten.opSimp.simplifyTerm === x)
    assert((x - (x - x + (x + x -x ))).flatten.opSimp.simplifyTerm.opSimp.simplifyTerm === Integer(0))
    assert((x**3**2).opSimp == x**6)
    assert((x*y*(y**2/z)).flatten.opSimp.opSimp.opSimp.flatten.opSimp.simplifyTerm.simplifyTerm === ((x*(y**3))/z).flatten)
    assert(((x/y)*(y/z)*(z/x)).flatten.opSimp.flatten.opSimp === Integer(1))
    assert(((x**2/y)*(y**2/z)*(z**2/x)).flatten.opSimp.opSimp.flatten.opSimp === (x*y*z).flatten)
  }

  test("Differentiation"){
    assert((1/x).diff(x).reduce === (-1/(x**2)))
    assert((y/x).diff(x).reduce === (-y/(x**2)).reduce)
    assert((x/x).diff(x).reduce === Integer(0))
    assert((y/x).diff(x).reduce === (-y/(x**2)))
    assert((x**10).diff(x, 10).reduce === Integer(720*7*8*9*10))
  }

  test("identities of expand"){
    assert((x).expand === x)

    assert((x+y).expand === (x+y))

    assert((x**2).expand === x**2)

    assert((x*y).expand.delIdentity.simplifyTerm === (x*y))
  }
  test("expand of Multiply"){
    assert(((x+y)*(x+y)).expand.reduce === (x**2 + 2*x*y + y**2).reduce)

    assert(((x+y)*(x+y)*(x+y)).expand.reduce === (x*x*x + 3*y*y*x + 3*x*x*y + y*y*y).reduce)

    assert(((x+y)*(x+y)*(x+y)).expand.reduce === (x**3 + 3*y*(x**2) + 3*x*(y**2) + y**3).reduce)

    assert(((x+y)*y*z).expand.reduce === (x*y*z + y*y*z).reduce)
  }
  test("expand of power"){
    assert(((x+y)**2).expand.reduce === (x**2 + 2*x*y + y**2).reduce)

    assert(((x+y)**3).expand.reduce === (x**3 + 3*y*(x**2) + 3*x*(y**2) + y**3).reduce)
  }
}
