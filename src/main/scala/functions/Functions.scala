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
  def flatten: Term = funcApply(arg1.flatten)

  /** Deletes the identity elements `id` with respect to  the operator `func` on
    * the argument(CTS) of Function */
  def delIdentity: Term = funcApply(arg1.delIdentity)

  /** If argument(CTS) of Function is of the form `CompositeTerm(BinOp(_), a::Nil)`
    * then return `a` otherwise the term itself */
  def singleTerm: Term = funcApply(arg1.singleTerm)

  /** Reduces the mulitiplicy of the same object in the argument(CTS) of Function
    * for example
    *
    * `f(x+x+x+x)` is converted to `f(4*x)` and
    * `f(x*x*x*x)` is converted to `f(x**4)`
    */
  def reduceMultiplicity: Term = funcApply(arg1.reduceMultiplicity)

  /** Reduces the f(CompositeTerm(BinOp("*"), List(...,Integer(0),...)))
    * to f(0) */
  def reduceMulZero: Term = funcApply(arg1.reduceMulZero)

  /** Rule 0: Simplify numbers with respect to corresponding operator
    * Rule 1: Keep Simplified Number as first term of list in CompositeTerm(BinOp("*"), _)
    * Rule 2: Keep Simplified Number as  last term of list in CompositeTerm(BinOp("+"), _)
    */
  def reduceNumber: Term = funcApply(arg1.reduceNumber)

  /** Reduces f(x - y - z) to f(x - (y+z)) and f(x*(-y)*(-z)) = f(x*y*z) */
  def reduceGroupNeg: Term = funcApply(arg1.reduceGroupNeg)

  /** Reduces expresstion like this f(abs(-x)) to f(abs(x)) */
  def reduceMinusAbs: Term = funcApply(arg1.reduceMinusAbs)

  // /** Recursively substitute `from` with `to` on the argument(CTS) of Function */
  // def recurSubs(from: Term, to: Term): Term = funcApply(arg1.recurSubs(from, to))

  def reduce: Term = funcApply(arg1.reduce)

  def expand: Term = funcApply(arg1.expand)

  def cancel: Term = funcApply(arg1.cancel)

  def subtract: Term = funcApply(arg1.subtract)

  def expandUnaryNeg: Term = funcApply(arg1.expandUnaryNeg)
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
  def flatten: Term =
    funcApply(arg1.flatten, arg2.flatten)

  /** Deletes the identity elements `id` with respect to  the operator `func` on
    * the argument(CTS) of Function */
  def delIdentity: Term =
    funcApply(arg1.delIdentity, arg2.delIdentity)

  /** If argument(CTS) of Function is of the form
    * `f(CompositeTerm(BinOp(_), a::Nil), y)` then return `f(a, y)` otherwise
    * the term itself */
  def singleTerm: Term = funcApply(arg1.singleTerm, arg2.singleTerm)

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
  def reduceMulZero: Term = funcApply(arg1.reduceMulZero, arg2.reduceMulZero)

  /** Rule 0: Simplify numbers with respect to corresponding operator
    * Rule 1: Keep Simplified Number as first term of list in CompositeTerm(BinOp("*"), _)
    * Rule 2: Keep Simplified Number as  last term of list in CompositeTerm(BinOp("+"), _)
    */
  def reduceNumber: Term = funcApply(arg1.reduceNumber, arg2.reduceNumber)

  /** Reduces f((x -y-z), a) to f((x - (y+z)), a) and
    * f((x*(-y)*(-z)), a) = f((x*y*z), a) */
  def reduceGroupNeg: Term = funcApply(arg1.reduceGroupNeg, arg2.reduceGroupNeg)

  /** Reduces expresstion like this f(abs(-x), y) to f(abs(x), y) */
  def reduceMinusAbs: Term = funcApply(arg1.reduceMinusAbs, arg2.reduceMinusAbs)

  // /** Recursively substitute `from` with `to` on the argument(CTS) of Function */
  // def recurSubs(from: Term, to: Term): Term =
  //   funcApply(arg1.recurSubs(from, to), arg2.recurSubs(from, to))

  def reduce: Term = funcApply(arg1.reduce, arg2.reduce)

  def expand: Term = funcApply(arg1.expand, arg2.expand)

  def cancel: Term = funcApply(arg1.cancel, arg2.cancel)

  def subtract: Term = funcApply(arg1.subtract, arg2.subtract)

  def expandUnaryNeg: Term = funcApply(arg1.expandUnaryNeg, arg2.expandUnaryNeg)
}
