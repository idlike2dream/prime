package prime.core

sealed trait Expression

abstract class Operator(val degree: Int) extends Expression {

  /** Name of function */
  def op: String

  /** Substitute parameters in the predicate */
  def apply(args: List[Term]): Term = {
    assert(args.length == degree)
    CompositeTerm(this, args)
  }

  /** Substitute parameters in the function */
  def apply(args: Term*): Term = CompositeTerm(this, args.toList)

}

case class BinOp(op: String) extends Operator(2)

case class UnaryOp(op: String) extends Operator(1)

trait BasicOperators {

  /** Add two terms */
  def +(that: Term): Term

  /** Subtract two terms */
  def -(that: Term): Term

  /** Multiply two terms */
  def *(that: Term): Term

  /** Divide two terms */
  def /(that: Term): Term

  /** Raise this term to the power of that term */
  def **(that: Term): Term

  /** Absolute value of the term */
  def abs: Term

  /** Multiply with -1 */
  def unary_- : Term

  /** Differentiate with respect to  to independent variable x */
  def diff(x: Symbol): Term

  /** Differentiate n times with respect to . to independent variable x */
  def diff(x: Symbol, n: Int): Term

  /** Substitute "from" term with "to" term */
  def subs(from: Term, to: Term): Term
}

trait Term extends Expression with BasicOperators {

  /** Add two terms. If both terms are numbers then gives sum of those two
    * numbers */
  def +(that: Term) = (this, that) match {
    case (a: Number,b: Number) => a + b
    case _ => BinOp("+")(this, that)
  }

  /** Subtract two terms. If both terms are numbers then gives difference of
    * those two numbers */
  // Replaced case _ => BinOp("-")(this, that) with this + (-that)
  // I guess it helps in handling "-" better. Now all - operator can be treated
  // as + operations with right operand having UnaryOp("-")
  def -(that: Term) = (this, that) match {
    case (a: Number,b: Number) => a - b
    case _ => this + (-that)
  }

  /** Multiply two terms. If both terms are numbers then gives product of those
    * two numbers */
  def *(that: Term) = (this, that) match {
    case (a: Number,b: Number) => a * b
    case _ => BinOp("*")(this, that)
  }

  /** Divide two terms. If both terms are number then divides both of those two
    * numbers but the type of output depends on type of numbers given as arguments
    */
  def /(that: Term) = (this, that) match {
    case (a: Number,b: Number) => a / b
    case _ => BinOp("/")(this, that)
  }

  /** Raise power of one number to the other. If the exponent is 1, then term
    * itself is returned.*/
//  #TODO if (that == Integer(1)) this #MayNotBeCorrect Should be done inside
//  formatToString
//  #TODO Handle the exceptions when that==Integer(0)
  def **(that: Term) =
    if (that == Integer(1)) this
    else if (that == Integer(0)) Integer(1)
    else BinOp("**")(this, that)

  /** Absolute value of a term If the term is a number, then returns absolute
    * value of a number*/
  def abs: Term = this match {
    case b@ (_: Number) => b.abs
    case _ => UnaryOp("abs")(this)
  }

  /** Multiply with -1 */
  def unary_- : Term = UnaryOp("-")(this)

  /** Differentiate n times with respect to . to independent variable x */
  def diff(x: Symbol, n: Int): Term = {
    assert(n > 0, throw new Error("n can't less that 1"))
    def diffAcc(t: Term, s: Symbol, m: Int): Term = {
      if (m == 1) t.diff(x)
      else diffAcc(t.diff(x), s, m - 1)
    }
    diffAcc(this, x, n)
  }

  /** Replaces the `this` with `to` if `this == from` */
  def subs(from: Term, to: Term): Term =
    if (this == from) to
    else this

  def flatten: Term
  def delIdentity: Term
  def singleTerm: Term
  def reduceMultiple: Term
  def mulZero: Term
  def reduceNumber: Term
  def reduceGroupNeg: Term
  def reduceMinusAbs: Term
  def expandUnaryNeg: Term

  def reduce: Term
  def expand: Term
  def cancel: Term
  def subtract: Term

  def formatToString: String
}

trait AtomicTerm extends Term {

  def diff(x: Symbol): Term

  // For n>1 Differentiation of an atomic term just returns 0.
  override def diff(x: Symbol, n: Int): Term = {
    assert(n > 0, throw new Error("n can't less that 1"))
    if (n == 1) diff(x)
    else diff(x).diff(x)    //Should this be simply Integer(0)
  }

  /** Simply returns the atomic term itself */
  def flatten: Term = this

  /** Simply returns the atomic term itself */
  def delIdentity: Term = this

  /** Simply returns the atomic term itself */
  def singleTerm: Term = this

  /** Simply returns the atomic term itself */
  def reduceMultiple: Term = this

  /** Simply returns the atomic term itself */
  def mulZero: Term = this

  /** Simply returns the atomic term itself */
  def reduceNumber: Term = this

  /** Simply returns the atomic term itself */
  def reduceGroupNeg: Term = this

  /** Simply returns the atomic term itself */
  def reduceMinusAbs: Term = this

  /** Simply returns the atomic term itself */
  def reduce: Term = this

  /** Simply returns the atomic term itself */
  def expand: Term = this

  /** Simply returns the atomic term itself*/
  def cancel: Term = this

  /** Simply returns the atomic term itself*/
  def subtract: Term = this

  /** Simply returns the atomic term itself*/
  def expandUnaryNeg: Term = this

  /** Returns toString of the atomic term. Just added for convinience*/
  def formatToString: String = toString

}

case class CompositeTerm(f: Operator, args: List[Term]) extends Term {
  require(args != Nil, throw new Error("arguments can't be zero"))

  /** Flattens the CompositeTerm tree structure with respect to an associative
    * operator `func` only the top most layer.*/
  def flatten: Term = {
    def flatList(func: BinOp, a: List[Term], b: List[Term]): List[Term] = a match {
      case Nil => b
      case CompositeTerm(`func`, l) :: ls => flatList(func, ls, b++l)
      case l :: ls => flatList(func, ls, b++List(l))
    }
    def flatRecur(x: Term): Term = x match {
      case CompositeTerm(BinOp("+"), ar) => {
        val simpArgs = ar map {flatRecur(_)}
        val finalList = flatList(BinOp("+"), simpArgs, Nil)
        CompositeTerm(BinOp("+"), finalList)
      }
      case CompositeTerm(BinOp("*"), ar) => {
        val simpArgs = ar map {flatRecur(_)}
        val finalList = flatList(BinOp("*"), simpArgs, Nil)
        CompositeTerm(BinOp("*"), finalList)
      }
      case CompositeTerm(fu, ar) => {
        CompositeTerm(fu, ar map {flatRecur(_)})
      }
      case _ => x
    }
    flatRecur(this)
  }

  /** Deletes the identity elements `id` with respect to  the operator `func` on
    * the `CompositeTerm(func, _)` tree structure */
  def delIdentity: Term = {
    def delIdenInTerm(id: Term, ar: List[Term]): List[Term] = ar filter (_!=id) match {
      case Nil => id :: Nil
      case a => a
    }
    def delRecur(x: Term): Term = x match {
      case CompositeTerm(BinOp("+"), ar) => {
        val simpArgs = ar map {delRecur(_)}
        val finalList = delIdenInTerm(Integer(0), simpArgs)
        CompositeTerm(BinOp("+"), finalList)
      }
      case CompositeTerm(BinOp("*"), ar) => {
        val simpArgs = ar map {delRecur(_)}
        val finalList = delIdenInTerm(Integer(1), simpArgs)
        CompositeTerm(BinOp("*"), finalList)
      }
      case CompositeTerm(BinOp("/"), ar) => {
        if (ar(1) == Integer(1)) ar(0) else x  // Mandatory Becase One argument CTerm BinOp("/") can break things
      }                                        // This can be done even under singleTerm.
      case CompositeTerm(BinOp("**"), ar) => {
        if (ar(1) == Integer(1)) ar(0) else x  // Mandatory
      }
      case CompositeTerm(UnaryOp("-"), CompositeTerm(UnaryOp("-"), a :: Nil) :: Nil) => delRecur(a)
      case CompositeTerm(fu, ar) => CompositeTerm(fu, ar map {delRecur(_)})
      case _ => x
    }
    delRecur(this)
  }

  /** If CompositeTerm is of the form `CompositeTerm(BinOp(_), a::Nil)` then
    return `a` otherwise the term itself */
  def singleTerm: Term = {
    def singleReturn(func: BinOp, ar: List[Term], x: Term): Term = {
      val simpArgs = ar map {singleTermRecur(_)}
      simpArgs match {          // Nil is not necessary because ar can't be Nil so simpArgs can't be Nil as well.
        case a :: Nil => a
        case _ => CompositeTerm(func, simpArgs)
      }
    }
    def singleTermRecur(x: Term): Term = x match {
      case CompositeTerm(BinOp("+"), ar) => singleReturn(BinOp("+"), ar, x)
      case CompositeTerm(BinOp("*"), ar) => singleReturn(BinOp("*"), ar, x)
      case CompositeTerm(UnaryOp("-"), a :: Nil) => {
        if (a == Integer(0)) Integer(0) else x
      }
      case _ => x
    }
    singleTermRecur(this)
  }

  /** Reduces the mulitiplicy of the same object in the CompositeTerm tree Structure
    * for example
    *
    * `(x+x+x+x)` is converted to `4*x` and `(x*x*x*x)` is converted to `x**4`
    */
  def reduceMultiple: Term = {
    def reduceMultipleRecur(x: Term): Term = x match {
      case CompositeTerm(BinOp("+"), ar) => {
        val simpArgs = ar map {reduceMultipleRecur(_)}
        val finalList = simpArgs.groupBy(identity).map{
          case (x,ls) => {val n = ls.size; if(n==1) x else Integer(n)*x}
        }.toList
        CompositeTerm(BinOp("+"), finalList)
      }
      case CompositeTerm(BinOp("*"), ar) => {
        val simpArgs = ar map {reduceMultipleRecur(_)}
        val finalList = simpArgs.groupBy(identity).map{
          case (x,ls) => {val n = ls.size; if(n==1) x else x**Integer(n)}
        }.toList
        CompositeTerm(BinOp("*"), finalList)
      }
      case _ => x
    }
    reduceMultipleRecur(this)
  }

  /** Reduces the CompositeTerm(BinOp("*"), List(..., 0, ...))
    * to Integer(0) or CompositeTerm(BinOp("/"), 0 :: _ :: Nil) to 0  */
  def mulZero: Term = {
    def mulZeroRecur(x: Term): Term = x match {
      case CompositeTerm(BinOp("*"), ar) => {
        val simpArgs = ar map {mulZeroRecur(_)}
        if (simpArgs exists (_==Integer(0))) Integer(0) else CompositeTerm(BinOp("*"), simpArgs)
      }
      case CompositeTerm(BinOp("/"), ar) => {
        val simpArgs = ar map {mulZeroRecur(_)}
        if (simpArgs(0) == Integer(0)) Integer(0) else CompositeTerm(BinOp("/"), simpArgs)
      }
      case CompositeTerm(fu, ar) => {
        val simpArgs = ar map {mulZeroRecur(_)}
        CompositeTerm(fu, simpArgs)
      }
      case _ => x
    }
    mulZeroRecur(this)
  }

  /** Rule 0: Simplify numbers with respect to corresponding operator
    * Rule 1: Keep Simplified Number as first term of list in CompositeTerm(BinOp("*"), _)
    * Rule 2: Keep Simplified Number as  last term of list in CompositeTerm(BinOp("+"), _)
    */
  def reduceNumber: Term = f match {
    case BinOp("*") => {
      val (num, terms) = args partition {
        case _:Number => true;case _ => false }
      val number = ((Integer(1): Term) /: num) (_ * _)

      if (number == Integer(1)) CompositeTerm(BinOp("*"), number :: terms)
      else CompositeTerm(BinOp("*"), number :: terms)
    }
    case BinOp("+") => {
      val (num, terms) = args partition {
        case _:Number => true;case _ => false }
      val number = ((Integer(0): Term) /: num) (_ + _)

      if (number == Integer(0)) CompositeTerm(BinOp("+"), terms)
      else CompositeTerm(BinOp("+"), terms++List(number))
    }
    case _ => this
  }

  /** Reduces (x - y - z) to (x - (y+z)) and x*(-y)*(-z) = x*y*z */
  // #TODO Find the culprit that causes " - 0" here and there
  def reduceGroupNeg: Term = f match {
    case BinOp("+") => {
      val (neg, pos) = {
        def cmp(a: List[Term], b: List[CompositeTerm], c: List[Term]): (List[CompositeTerm], List[Term]) = a match {
          case Nil => (b, c)
          case (x @ CompositeTerm(UnaryOp("-"), _)) :: xs => cmp(xs, b ++ List(x), c)
          case x :: xs => cmp(xs, b, c++List(x))
        }
        cmp(args, Nil, Nil)
      }
      val newNeg = neg map {_.args(0)}
      val negList = CompositeTerm(BinOp("+"), newNeg)::Nil
      CompositeTerm(f, pos++List(CompositeTerm(UnaryOp("-"), negList)))
    }
    case BinOp("*") => {
      val (neg, pos) = {
        def cmp(a: List[Term], b: List[CompositeTerm], c: List[Term]): (List[CompositeTerm], List[Term]) = a match {
          case Nil => (b, c)
          case (x @ CompositeTerm(UnaryOp("-"), _)) :: xs => cmp(xs, b ++ List(x), c)
          case x :: xs => cmp(xs, b, c++List(x))
        }
        cmp(args, Nil, Nil)
      }
      val l = neg map {_.args(0)}
      // Assumption that reduceNumber is already done if no number is present
      // by default 1 is assumed.
      val (n, t): (Number, List[Term]) = pos match {
        case (a: Number) :: _ => (a, pos.tail)
        case _ => (Integer(1), pos)
      }
      if ((l.length%2 == 0 && n.signum==1)||(l.length % 2==1 && n.signum == -1))
        CompositeTerm(f, List(n.abs) ++ t ++ l)
      else
        CompositeTerm(UnaryOp("-"), CompositeTerm(f, List(n.abs) ++ t ++ l)::Nil)
    }
    case _ => this
  }

  // #TODO Write  distributive method which does `a*(b+c)*d` to `a*b*d + a*c*d`
  // This doesn't necessarily depend on Ordering because of it's definition.
  // Even a corresponding recur term

  /** Reduces expresstion like this abs(-x) to abs(x) */
  def reduceMinusAbs: Term = (f, args) match {
    case (UnaryOp("abs"), CompositeTerm(UnaryOp("-"), a::Nil)::Nil) => a
    case _ => this
  }

  // TestExpression.scala tests fail when recurRedMul is included inside.
  // So this exists as a seperate entity

  // #TODO Why is lazy val required for reduce and reducePartial?? val doesn't work

  // #TODO May be reduce should be written in a recursive way like
  // reducePartial.(recurRedMul.reducePartial)*n reduce until the
  // CompositeTerm no longer simplifies
  // Changing the order of functions creates


  lazy val reduce: Term = this

  // expand((x+y)*(x+y)) => (x**2 + 2*x*y + y**2)
  def expand: Term = {

    def expandHere(y: Term): Term = y match {
      case CompositeTerm(BinOp("*"), args) => {

        // sprAdd seperate terms like (x+y), (y+z) from (x+y)*(y+z)*y*(a+b)
        def sprAdd(a: List[Term], b: List[CompositeTerm], c: List[Term]): (List[CompositeTerm], List[Term]) = a match {
          case Nil => (b, c)
          case (x@ CompositeTerm(BinOp("+"), _)) :: xs =>
            sprAdd(xs, b ++ List(x), c)
          case x :: xs => sprAdd(xs, b, c ++ List(x))
        }

        //Flattens (a+b)(c+d) => (a*c+ a*d + b*c + c*d)
        def additiveMul(a: List[CompositeTerm]): CompositeTerm = a match {
          case x :: Nil => x
          case x :: y :: xs => {
            val newArg: List[Term] = for (i <- x.args; j <- y.args) yield i*j
            val newTerm = CompositeTerm(BinOp("+"), newArg)
            additiveMul(newTerm :: xs)
          }
        }

        // Flattens (a+b)*c => (a*c + b*c)
        def pointMul(a: CompositeTerm, b: List[Term]): Term = {
          val listMul =
            (a.args map {case x => x :: b}) map { CompositeTerm(BinOp("*"), _) }
          CompositeTerm(BinOp("+"), listMul)
        }

        sprAdd(args, Nil, Nil) match {
          case (adds, Nil) =>
            additiveMul(adds)
          case (Nil, terms) =>
            pointMul(CompositeTerm(BinOp("+"), Integer(1)::Nil), terms)
          case (adds, terms)  =>
            pointMul(additiveMul(adds), terms)
        }
      }
      case CompositeTerm(BinOp("**"), a :: (n: Integer) :: Nil) => {
        val args = List.fill(n.arg1.toInt)(a)
        expandHere(CompositeTerm(BinOp("*"), args))
      }
      case _ => y
    }

    def expandRecur(y: Term): Term = y match {
      case CompositeTerm(BinOp("*"), a) =>
        expandHere(CompositeTerm(BinOp("*"), a map {expandRecur(_)} ))
      case CompositeTerm(BinOp("**"), a :: (n: Integer) :: Nil) => {
        val args = List.fill(n.arg1.toInt)(a)
        expandHere(CompositeTerm(BinOp("*"), args map {expandRecur(_)}))
      }
      case _ => y
    }

    expandRecur(this)
  }

  def cancel: Term = {

    def cancelTerm(x: Term): Term = x match {
      case CompositeTerm(BinOp("+"), list) => {
        def cancelAdd(list: List[Term], acc: List[Term]): List[Term] = list match {
          case Nil => acc match {case Nil => Integer(0) :: Nil ; case _ => acc }
          case x :: xs => {
            val negX = CompositeTerm(UnaryOp("-"), x :: Nil)
            if (xs exists {case `negX` => true; case _ => false}) {
              val newList = (xs.toBuffer -= negX).toList
              cancelAdd(newList, acc)
            }
            else cancelAdd(xs, acc ++ List(x))
          }
        }
        CompositeTerm(BinOp("+"), cancelAdd(list, Nil))
      }
      case _ => x
    }

    def recCancel(x: Term): Term = x match {
      case CompositeTerm(func, arg) =>
        cancelTerm(CompositeTerm(func, arg map {recCancel(_)}))
      case _ => x
    }

    recCancel(this)
  }

  def subtract: Term = this

  def expandUnaryNeg: Term = (f, args) match {
    case (BinOp("-"), CompositeTerm(BinOp("+"), x):: Nil) => {
      CompositeTerm(BinOp("+"), x map {y => CompositeTerm(UnaryOp("-"), List(y))})
    }
    case _ => this
  }

  /** Differentiate the CompositeTerm with respect to an independent variable `x`*/
  // #TODO write cases for operators abs, /
  def diff(x: Symbol): Term = f match {
    case BinOp("+") | UnaryOp("-") => CompositeTerm(f, args.map(_.diff(x)))
    case BinOp("*") => {
      val a = args.zipWithIndex.map {
        case (_, i) =>
          args.zipWithIndex.map{case (xs, j) => if (i == j) xs.diff(x) else xs}
      }
      CompositeTerm(BinOp("+"), a.map(CompositeTerm(f, _)))
    }
    case BinOp("**") => {
      args(1) match {
        case a: Integer => {
          val list = List(a, args(0)**(a-Integer(1)), args(0).diff(x))
          CompositeTerm(BinOp("*"), list)
        }
        case _ => BinOp("diff")(this, x)
      }
    }
    case BinOp("/") => {
      val u = args(0); val v = args(1)
      if (u == Integer(1)) (-(v.diff(x))) / (v**Integer(2))
      else (v * u.diff(x) -  u * v.diff(x)) / (v**Integer(2))
    }
    case _ => BinOp("diff")(this, x)
  }

  override def diff(x: Symbol, n: Int): Term = {
    assert(n > 0, throw new Error("n can't less that 1"))
    def diffAcc(t: Term, s: Symbol, m: Int): Term = {
      if (m == 1) t.diff(x)
      else diffAcc(t.diff(x).reduce, s, m - 1)
    }
    diffAcc(this, x, n)
  }

  // #TODO Need to write a special Printing module.
  override def formatToString = f match {
    case UnaryOp(op) => op match {
      case "-" => op + args(0)
      case  _  => op + "("+args(0)+")"
    }
    case BinOp(op)   => (args, op) match {
      case ( Nil,   _) => ""
      case (x::Nil, _) => x.toString
      case (_, "+") => args.zipWithIndex.map{
        case (CompositeTerm(UnaryOp("-"), x:: Nil), _) =>" - " + x
        case x@ _ => if(x._2 == 0) x._1.toString else " + "+x._1
      }.mkString("(", "", ")")
      case (_, "*") | (_, "/")  => args.mkString("(", op, ")")
      case (_,  "**" ) => args.mkString(op)
      case (_, "diff") => op+"("+args(0)+", "+ args(1)+")"
      case _ => args.mkString("(", op+" ", ")")
    }
  }

  // #TODO Write Ordering Rules to order the terms in with respect to
  // descending order of degree of term. (Only works for Single variable polys)
  // Should think of general rules May be based on Lexicographic Ordering in
  // Some Strange way.

  // #TODO After writing a pretty Printing module Construct a way to initailize
  // a global variable Which will turn on the pretty Printing by default.

//  override def toString = formatToString

  /** Experimenting with `==` Not final yet. */
  override def equals(a: Any) = a match {
    case CompositeTerm(`f`, ar) => (ar.groupBy(identity) == args.groupBy(identity))
    case _ => false
  }


}

object implicits {
  import scala.language.implicitConversions

  implicit def intToNumber(x: Int) = Integer(x)
  implicit def doubleToNumber(x: Double) = Real(x)
}
