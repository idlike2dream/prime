package prime.functions

import prime.core._

sealed trait Function extends Term

trait ElemementaryFunction extends Term

trait SpecialFunction extends Term

trait CombinatorialFunction extends Term

trait Univariate extends Term {

  def arg1: Term

  def funcApply(t: Term): Term

////////////////////////////////////////////////////////////////////////////////

  def flatten(func: BinOp): Term = funcApply(arg1.flatten(func))

  def delId(f: BinOp, id: Term): Term = funcApply(arg1.delId(f, id))

  def reduceToSingle: Term = funcApply(arg1.reduceToSingle)

  def reduceMultiplicity: Term = funcApply(arg1.reduceMultiplicity)

  def reduceMulZero: Term = funcApply(arg1.recurMulZero)

  def reduceNumber: Term = funcApply(arg1.reduceNumber)

  def reduceUnaryNeg: Term = funcApply(arg1.reduceUnaryNeg)

  def reduceGroupNeg: Term = funcApply(arg1.reduceGroupNeg)

  def reduceMinusAbs: Term = funcApply(arg1.reduceMinusAbs)

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

  def reducePartial: Term = funcApply(arg1.reducePartial)

  def reduce: Term = funcApply(arg1.reduce)

}

trait Bivariate extends Term {

  def arg1: Term
  def arg2: Term

  def funcApply(t1: Term, t2: Term): Term

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

  def reducePartial: Term = funcApply(arg1.reducePartial, arg2.reducePartial)

  def reduce: Term = funcApply(arg1.reduce, arg2.reduce)

}
