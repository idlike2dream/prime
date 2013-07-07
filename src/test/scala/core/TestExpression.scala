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
  test("singleTerm") {
    val a = (0+(0+(0+x))).flatten.delIdentity
    val b = (y*1*(1*(0+(0+(1*1*x))))).flatten.delIdentity

    assert(a.singleTerm === x)
    assert(b.singleTerm === x*y)
  }
  test("reduceMultiplicity") {

    assert((x+x+x+y+y+z).flatten.reduceMultiplicity === (3*x + 2*y +z).flatten)
    assert((x*x*x*y*y*z).flatten.reduceMultiplicity === ((x**3)*(y**2)*z).flatten)
  }


 // test("Division Differentiation"){
 //   assert((1/x).diff(x) === (-1/(x**2)))
 //   assert((y/x).diff(x) === (-y/(x**2)))
 //   assert((x/x).diff(x).cancel === Integer(0))
 //   //assert((y/x).diff(x) === y*(-1/(x**2)))
 // }
 test("Cancel"){
   assert((x-x).cancel.singleTerm === Integer(0))
    assert((x-x+y-y).flatten.cancel.singleTerm === Integer(0))
    assert((2*x - 2*x).cancel.singleTerm === Integer(0))
   // assert((x*(y -y)).cancel === Integer(0))
    assert((x-x+x).flatten.cancel.singleTerm === x)
 }
 test("identities of expand"){
   assert((x).expand === x)

   assert((x+y).expand === (x+y))

   assert((x**2).expand.reduceMultiplicity.delIdentity.singleTerm === x**2)

   assert((x*y).expand.delIdentity.singleTerm === (x*y))
 }
//  test("expand of Multiply"){
//    assert(((x+y)*(x+y)).expand === (x**2 + 2*x*y + y**2))

//    assert(((x+y)*(x+y)*(x+y)).expand === (x*x*x + 3*y*y*x + 3*x*x*y + y*y*y))

//    // fails without reduce on both sides And wierdly Why is there a "- 0" in
//    // both sides when reduce is not present on either sides
//    // ((3*x*y**2) + x**3 + y**3 + (3*y*x**2) - 0) did not equal
//    // (x**3 + y**3 + (3*y*x**2) + (3*x*y**2) - 0)
//    assert(((x+y)*(x+y)*(x+y)).expand.reduce === (x**3 + 3*y*(x**2) + 3*x*(y**2) + y**3).reduce)

//    assert(((x+y)*y*z).expand === (x*y*z + y*y*z))
//  }
//  test("expand of power"){
//    assert(((x+y)**2).expand === (x**2 + 2*x*y + y**2))

//    assert(((x+y)**3).expand.reduce === (x**3 + 3*y*(x**2) + 3*x*(y**2) + y**3).reduce)
//  }
}
