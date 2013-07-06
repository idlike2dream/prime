package prime.functions

import prime.core._

//  Implementation Details
//
//  Function are divided divided under two classification.
//
//  1st one (Based on ???)
//    - ElemementaryFunction, https://en.wikipedia.org/wiki/Elementary_function
//    - SpecialFunction (erf, gamma, Beta, polygamma, etc., )
//    - CombinatorialFunction (nCk, nPk, factorial, etc., )
//
//  2nd one (Based on number of arguments)
//    - Univariate
//    - Bivariate
//
//  The 1st classification is necessary because I plan to work on Risch
//  Algorithm for symbolic Integration. 2nd classification is necessary
//  Because I have defined several simplification methods like recurFlatten
//  reduceNumber, recurDelId, etc., inside the trait Term. When it comes to
//  trait Function, their implementation essentially depends on no. of arguments
//  of the functions itself. So this will reduce a huge amount of boilerplate.


// #TODO have to implement the Differentiation of composition of function using
// super.diff(x)

sealed trait Function extends Term

trait ElemementaryFunction extends Function

trait SpecialFunction extends Function

trait CombinatorialFunction extends Function

trait Univariate extends Function {

  def arg1: Term

  // This function removes the boilerplate. It's implementation is something like
  // this def funcApply(t: Term) = Exp(t) inside Exp case class.

  protected def funcApply(t: Term): Term

  // Argument Simplification Methods.

  // CompositeTerm Tree Structure => CTS
  /** Flattens the argument(CTS) of Function with respect to an associative
    * operator `func` only the top most layer.*/
  def flatten(func: BinOp): Term = funcApply(arg1.flatten(func))

  /** Deletes the identity elements `id` with respect to  the operator `func` on
    * the argument(CTS) of Function */
  def delId(f: BinOp, id: Term): Term = funcApply(arg1.delId(f, id))

  /** If argument(CTS) of Function is of the form `CompositeTerm(BinOp(_), a::Nil)`
      then return `a` otherwise the term itself */
  def reduceToSingle: Term = funcApply(arg1.reduceToSingle)

  /** Reduces the mulitiplicy of the same object in the argument(CTS) of Function
    * for example
    *
    * `f(x+x+x+x)` is converted to `f(4*x)` and
    * `f(x*x*x*x)` is converted to `f(x**4)`
    */
  def reduceMultiplicity: Term = funcApply(arg1.reduceMultiplicity)

  /** Reduces the f(CompositeTerm(BinOp("*"), List(...,Integer(0),...)))
    * to f(0) */
  def reduceMulZero: Term = funcApply(arg1.recurMulZero)

  /** Rule 0: Simplify numbers with respect to corresponding operator
    * Rule 1: Keep Simplified Number as first term of list in CompositeTerm(BinOp("*"), _)
    * Rule 2: Keep Simplified Number as  last term of list in CompositeTerm(BinOp("+"), _)
    */
  def reduceNumber: Term = funcApply(arg1.reduceNumber)

  /** Reduces f(-(-x)) to f(x) */
  def reduceUnaryNeg: Term = funcApply(arg1.reduceUnaryNeg)

  /** Reduces f(x - y - z) to f(x - (y+z)) and f(x*(-y)*(-z)) = f(x*y*z) */
  def reduceGroupNeg: Term = funcApply(arg1.reduceGroupNeg)

  /** Reduces expresstion like this f(abs(-x)) to f(abs(x)) */
  def reduceMinusAbs: Term = funcApply(arg1.reduceMinusAbs)

  /** Recursively substitute `from` with `to` on the argument(CTS) of Function */
  def recurSubs(from: Term, to: Term): Term = funcApply(arg1.recurSubs(from, to))

  /** Recursively flattens the argument(CTS) of Function with respect to  to
    * an associative operator `func` */
  def recurFlatten(f: BinOp): Term = funcApply(arg1.recurFlatten(f))

  /** Recursively deletes the identity elements `id` with respect to  the operator
    * `func` on the argument(CTS) of Function */
  def recurDelId(f: BinOp, id: Term): Term = funcApply(arg1.recurDelId(f, id))

  /** Recursively convert `f(CompositeTerm(BinOp(_), a::Nil))` to `f(a)` on the
    * argument(CTS) of Function */
  def recurSingle: Term = funcApply(arg1.recurSingle)

  /** Recursively reduces the mulitiplicy of the same object in the argument(CTS)
    *  of Function using reduceMultiplicity */
  def recurRedMul: Term = funcApply(arg1.recurRedMul)

  /** Recursively reduces the f(CompositeTerm(BinOp("*"), List(...,Integer(0),...)))
    * to f(0) on the CompositeTerm Tree Structure */
  def recurMulZero: Term = funcApply(arg1.recurMulZero)

  /** Recursively Reduces the argument(CTS) of Function according to
    * Rule 0, 1, 2 using reduceNumber */
  def recurRedNum: Term = funcApply(arg1.recurRedNum)

  /** Recursively Reduces Terms of form f(-(-x)) to f(x) on the argument(CTS) of
    * Function */
  def recurUnaryNeg: Term = funcApply(arg1.recurUnaryNeg)

  /** Recursively reduces f(x-y-z) to f(x-(y+z)) on the argument(CTS) of
    * Function */
  def recurGroupNeg: Term = funcApply(arg1.recurGroupNeg)

  /** Recursively reduces expresstion like this f(abs(-x)) to f(abs(x)) */
  def recurMinusAbs: Term = funcApply(arg1.recurMinusAbs)

  def reducePartial: Term = funcApply(arg1.reducePartial)

  def reduce: Term = funcApply(arg1.reduce)

  def expand: Term = funcApply(arg1.expand)

  def cancel: Term = funcApply(arg1.cancel)

  def expandUnaryNeg: Term = funcApply(arg1.expandUnaryNeg)

  def recExUnaryNeg: Term = funcApply(arg1.recExUnaryNeg)

}

trait Bivariate extends Function {

  def arg1: Term
  def arg2: Term

  def funcApply(t1: Term, t2: Term): Term

  // The simplification is only written for first argument but it also holds for
  // second argument as well.

  // CompositeTerm Tree Structure => CTS
  /** Flattens the argument(CTS) of Function with respect to an associative
    * operator `func` only the top most layer.*/
  def flatten(func: BinOp): Term =
    funcApply(arg1.flatten(func), arg2.flatten(func))

  /** Deletes the identity elements `id` with respect to  the operator `func` on
    * the argument(CTS) of Function */
  def delId(f: BinOp, id: Term): Term =
    funcApply(arg1.delId(f, id), arg2.delId(f, id))

  /** If argument(CTS) of Function is of the form
    * `f(CompositeTerm(BinOp(_), a::Nil), y)` then return `f(a, y)` otherwise
    * the term itself */
  def reduceToSingle: Term = funcApply(arg1.reduceToSingle, arg2.reduceToSingle)

  /** Reduces the mulitiplicy of the same object in the argument(CTS) of Function
    * for example
    *
    * `f(x+x+x+x, y)` is converted to `f(4*x, y)` and
    * `f(x*x*x*x, y)` is converted to `f(x**4, y)`
    */
  def reduceMultiplicity: Term =
    funcApply(arg1.reduceMultiplicity, arg2.reduceMultiplicity)

  /** Reduces the f(CompositeTerm(BinOp("*"), List(...,Integer(0),...)), a)
    * to f(0, a) */
  def reduceMulZero: Term = funcApply(arg1.recurMulZero, arg2.recurMulZero)

  /** Rule 0: Simplify numbers with respect to corresponding operator
    * Rule 1: Keep Simplified Number as first term of list in CompositeTerm(BinOp("*"), _)
    * Rule 2: Keep Simplified Number as  last term of list in CompositeTerm(BinOp("+"), _)
    */
  def reduceNumber: Term = funcApply(arg1.reduceNumber, arg2.reduceNumber)

  /** Reduces f(-(-x), y) to f(x, y) */
  def reduceUnaryNeg: Term = funcApply(arg1.reduceUnaryNeg, arg2.reduceUnaryNeg)

  /** Reduces f((x -y-z), a) to f((x - (y+z)), a) and
    * f((x*(-y)*(-z)), a) = f((x*y*z), a) */
  def reduceGroupNeg: Term = funcApply(arg1.reduceGroupNeg, arg2.reduceGroupNeg)

  /** Reduces expresstion like this f(abs(-x), y) to f(abs(x), y) */
  def reduceMinusAbs: Term = funcApply(arg1.reduceMinusAbs, arg2.reduceMinusAbs)

  /** Recursively substitute `from` with `to` on the argument(CTS) of Function */
  def recurSubs(from: Term, to: Term): Term =
    funcApply(arg1.recurSubs(from, to), arg2.recurSubs(from, to))

  /** Recursively flattens the argument(CTS) of Function with respect to  to
    * an associative operator `func` */
  def recurFlatten(f: BinOp): Term =
    funcApply(arg1.recurFlatten(f), arg2.recurFlatten(f))

  /** Recursively deletes the identity elements `id` with respect to  the operator
    * `func` on the argument(CTS) of Function */
  def recurDelId(f: BinOp, id: Term): Term =
    funcApply(arg1.recurDelId(f, id), arg2.recurDelId(f, id))

  /** Recursively convert `f(CompositeTerm(BinOp(_), a::Nil), y)` to `f(a, y)`
    * on the argument(CTS) of Function */
  def recurSingle: Term = funcApply(arg1.recurSingle, arg2.recurSingle)

  /** Recursively reduces the mulitiplicy of the same object in the argument(CTS)
    * of Function using reduceMultiplicity */
  def recurRedMul: Term = funcApply(arg1.recurRedMul, arg2.recurRedMul)

  /** Recursively reduces the f(CompositeTerm(BinOp("*"), List(...,0,...)), y)
    * to f(0, y) on the argument(CTS) of Function */
  def recurMulZero: Term = funcApply(arg1.recurMulZero, arg2.recurMulZero)

  /** Recursively Reduces the argument(CTS) of Function according to
    * Rule 0, 1, 2 using reduceNumber */
  def recurRedNum: Term = funcApply(arg1.recurRedNum, arg2.recurRedNum)

  /** Recursively Reduces Terms of form f(-(-x), y) to f(x, y) on the CTS */
  def recurUnaryNeg: Term = funcApply(arg1.recurUnaryNeg, arg2.recurUnaryNeg)

  /** Recursively reduces f((x-y-z), a) to f((x-(y+z)), a) on the CTS */
  def recurGroupNeg: Term = funcApply(arg1.recurGroupNeg, arg2.recurGroupNeg)

  /** Recursively reduces expresstion like this f(abs(-x), y) to f(abs(x), y)
    * */
  def recurMinusAbs: Term = funcApply(arg1.recurMinusAbs, arg2.recurMinusAbs)

  def reducePartial: Term = funcApply(arg1.reducePartial, arg2.reducePartial)

  def reduce: Term = funcApply(arg1.reduce, arg2.reduce)

  def expand: Term = funcApply(arg1.expand, arg2.expand)

  def cancel: Term = funcApply(arg1.cancel, arg2.cancel)

  def expandUnaryNeg: Term = funcApply(arg1.expandUnaryNeg, arg2.expandUnaryNeg)

  def recExUnaryNeg: Term = funcApply(arg1.recExUnaryNeg, arg2.recExUnaryNeg)

}
