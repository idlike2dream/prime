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

sealed trait Function extends Term

trait ElemementaryFunction extends Function

trait SpecialFunction extends Function

trait CombinatorialFunction extends Function

trait Univariate extends Function {

  def arg1: Term

  // This function removes the boilerplate. It's implementation is something like
  // this def funcApply(t: Term) = Exp(t) inside Exp case class.

  def funcApply(t: Term): Term

  def flatten(func: BinOp): Term = funcApply(arg1.flatten(func))

  def delId(f: BinOp, id: Term): Term = funcApply(arg1.delId(f, id))

  def reduceToSingle: Term = funcApply(arg1.reduceToSingle)

  def reduceMultiplicity: Term = funcApply(arg1.reduceMultiplicity)

  def reduceMulZero: Term = funcApply(arg1.recurMulZero)

  def reduceNumber: Term = funcApply(arg1.reduceNumber)

  def reduceUnaryNeg: Term = funcApply(arg1.reduceUnaryNeg)

  def reduceGroupNeg: Term = funcApply(arg1.reduceGroupNeg)

  def reduceMinusAbs: Term = funcApply(arg1.reduceMinusAbs)

  def recurSubs(from: Term, to: Term): Term = funcApply(arg1.recurSubs(from, to))

  def recurFlatten(f: BinOp): Term = funcApply(arg1.recurFlatten(f))

  def recurDelId(f: BinOp, id: Term): Term = funcApply(arg1.recurDelId(f, id))

  def recurSingle: Term = funcApply(arg1.recurSingle)

  def recurRedMul: Term = funcApply(arg1.recurRedMul)

  def recurMulZero: Term = funcApply(arg1.recurMulZero)

  def recurRedNum: Term = funcApply(arg1.recurRedNum)

  def recurUnaryNeg: Term = funcApply(arg1.recurUnaryNeg)

  def recurGroupNeg: Term = funcApply(arg1.recurGroupNeg)

  def recurMinusAbs: Term = funcApply(arg1.recurMinusAbs)

  def reducePartial: Term = funcApply(arg1.reducePartial)

  def reduce: Term = funcApply(arg1.reduce)

}

trait Bivariate extends Function {

  def arg1: Term
  def arg2: Term

  def funcApply(t1: Term, t2: Term): Term

  def flatten(func: BinOp): Term =
    funcApply(arg1.flatten(func), arg2.flatten(func))

  def delId(f: BinOp, id: Term): Term =
    funcApply(arg1.delId(f, id), arg2.delId(f, id))

  def reduceToSingle: Term = funcApply(arg1.reduceToSingle, arg2.reduceToSingle)

  def reduceMultiplicity: Term =
    funcApply(arg1.reduceMultiplicity, arg2.reduceMultiplicity)

  def reduceMulZero: Term = funcApply(arg1.recurMulZero, arg2.recurMulZero)

  def reduceNumber: Term = funcApply(arg1.reduceNumber, arg2.reduceNumber)

  def reduceUnaryNeg: Term = funcApply(arg1.reduceUnaryNeg, arg2.reduceUnaryNeg)

  def reduceGroupNeg: Term = funcApply(arg1.reduceGroupNeg, arg2.reduceGroupNeg)

  def reduceMinusAbs: Term = funcApply(arg1.reduceMinusAbs, arg2.reduceMinusAbs)

  def recurSubs(from: Term, to: Term): Term =
    funcApply(arg1.recurSubs(from, to), arg2.recurSubs(from, to))

  def recurFlatten(f: BinOp): Term =
    funcApply(arg1.recurFlatten(f), arg2.recurFlatten(f))

  def recurDelId(f: BinOp, id: Term): Term =
    funcApply(arg1.recurDelId(f, id), arg2.recurDelId(f, id))

  def recurSingle: Term = funcApply(arg1.recurSingle, arg2.recurSingle)

  def recurRedMul: Term = funcApply(arg1.recurRedMul, arg2.recurRedMul)

  def recurMulZero: Term = funcApply(arg1.recurMulZero, arg2.recurMulZero)

  def recurRedNum: Term = funcApply(arg1.recurRedNum, arg2.recurRedNum)

  def recurUnaryNeg: Term = funcApply(arg1.recurUnaryNeg, arg2.recurUnaryNeg)

  def recurGroupNeg: Term = funcApply(arg1.recurGroupNeg, arg2.recurGroupNeg)

  def recurMinusAbs: Term = funcApply(arg1.recurMinusAbs, arg2.recurMinusAbs)

  def reducePartial: Term = funcApply(arg1.reducePartial, arg2.reducePartial)

  def reduce: Term = funcApply(arg1.reduce, arg2.reduce)

}
