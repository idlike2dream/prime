package prime.core

sealed trait Expression

abstract class Function(val degree: Int) extends Expression {
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

case class BinOp(op: String) extends Function(2)

case class UnaryOp(op: String) extends Function(1)

trait Operators {

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

trait Term extends Expression with Operators {

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

  /** Divide two terms. If both terms are number then divides both of those two numbers
    * but the type of output depends on type of numbers given as arguments
    */
  def /(that: Term) = (this, that) match {
    case (a: Number,b: Number) => a / b
    case _ => BinOp("/")(this, that)
  }

  /** Raise power of one number to the other. If the exponent is 1, then term
    * itself is returned.*/
//    if (that == Integer(1)) this #MayNotBeCorrect Should be done inside
//    formatToString
  def **(that: Term) =
    if (that == Integer(1)) this
    else BinOp("**")(this, that)

  /** Absolute value of a term If the term is a number, then returns absolute
    * value of a number*/
  def abs: Term = this match {
    case b@ (_: Number) => b.abs
    case _ => UnaryOp("abs")(this)
  }

  /** Multiply with -1 */
  // Should this be Integer(-1)*this?? But this makes life diffculut while
  //  handling non-associativeness of - I guess.
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

  /** Has no meaning here Only added for the sake of convinience.
    * Makes sense only in the context of CompositeTerm
    */
  def args: List[Term]

  def flatten(func: BinOp): Term
  def delId(f: BinOp, id: Term): Term
  def reduceToSingle: Term
  def reduceMultiplicity: Term
  def reduceMulZero: Term
  def reduceNumber: Term
  def reduceUnaryNeg: Term
  def reduceGroupNeg: Term

  def recurFlatten(f: BinOp): Term
  def recurDelId(f: BinOp, id: Term): Term
  def recurSingle: Term
  def recurRedMul: Term
  def recurMulZero: Term
  def recurRedNum: Term
  def recurUnaryNeg: Term
  def recurGroupNeg: Term

  def reducePartial: Term
  def reduce: Term

  def formatToString: String
}

trait AtomicTerm extends Term {
  /** Has no meaning here Only added for the sake of convinience */
  def args: List[Term] = throw new Error("No arguments in atomic term")

  def subs(from: Term, to: Term): Term =
    if (this == from) to
    else this

  def diff(x: Symbol): Term

  // For n>1 Differentiation of an atomic term just returns 0.
  override def diff(x: Symbol, n: Int): Term = {
    assert(n > 0, throw new Error("n can't less that 1"))
    if (n == 1) diff(x)
    else diff(x).diff(x)
  }

  /** Simply returns the atomic term itself */
  def flatten(func: BinOp): Term = this

  /** Simply returns the atomic term itself */
  def delId(f: BinOp, id: Term): Term = this

  /** Simply returns the atomic term itself */
  def reduceToSingle: Term = this

  /** Simply returns the atomic term itself */
  def reduceMultiplicity: Term = this

  /** Simply returns the atomic term itself */
  def reduceMulZero: Term = this

  /** Simply returns the atomic term itself */
  def reduceNumber: Term = this

  /** Simply returns the atomic term itself */
  def reduceUnaryNeg: Term = this

  /** Simply returns the atomic term itself */
  def reduceGroupNeg: Term = this

  /** Simply returns the atomic term itself */
  def recurFlatten(f: BinOp): Term = this

  /** Simply returns the atomic term itself */
  def recurDelId(f: BinOp, id: Term): Term = this

  /** Simply returns the atomic term itself */
  def recurSingle: Term = this

  /** Simply returns the atomic term itself */
  def recurRedMul: Term = this

  /** Simply returns the atomic term itself */
  def recurMulZero: Term = this

  /** Simply returns the atomic term itself */
  def recurRedNum: Term = this

  /** Simply returns the atomic term itself */
  def recurUnaryNeg: Term = this

  /** Simply returns the atomic term itself */
  def recurGroupNeg: Term = this

  /** Simply returns the atomic term itself */
  def reducePartial: Term = this

  /** Simply returns the atomic term itself */
  def reduce: Term = this

  /** Returns toString of the atomic term. Just added for convinience*/
  def formatToString: String = toString

}

case class CompositeTerm(f: Function, args: List[Term]) extends Term {

  //#TODO write recurSubs
  def subs (from: Term, to: Term): Term = CompositeTerm(f, args map { _.subs(from, to) })

  /** Recursively flattens the CompositeTerm tree Structure with respect to  to
    * an associative operator `func` */
  def recurFlatten(func: BinOp): Term =
    CompositeTerm(f, args map (_.recurFlatten(func))).flatten(func)

  /** Recursively deletes the identity elements `id` with respect to  the operator
    * `func` on the CompositeTerm tree structure */
  def recurDelId(func: BinOp, id: Term): Term =
    CompositeTerm(f, args map (_.recurDelId(func, id))).delId(func, id)

  /** Recursively convert `CompositeTerm(BinOp(_), a::Nil)` to `a` on the
    * CompositeTerm tree structure */
  def recurSingle: Term =
    CompositeTerm(f, args map (_.recurSingle)).reduceToSingle

  /** Recursively reduces the mulitiplicy of the same object in the CompositeTerm
    * tree Structure using reduceMultiplicity */
  def recurRedMul: Term =
    CompositeTerm(f, args map (_.recurRedMul)).reduceMultiplicity

  /** Recursively reduces the CompositeTerm(BinOp("*"), List(...,Integer(0),...))
    * to Integer(0) on the CompositeTerm Tree Structure */
  def recurMulZero: Term =
    CompositeTerm(f, args map (_.recurMulZero)).reduceMulZero

  /** Recursively Reduces the CompositeTerm tree structures according to
    * Rule 0, 1, 2 using reduceNumber */
  def recurRedNum: Term =
    CompositeTerm(f, args map (_.recurRedNum)).reduceNumber

  /** Recursively Reduces Terms of form -(-x) to x on the CompositeTerm tree
    * structure */
  def recurUnaryNeg: Term =
    CompositeTerm(f, args map (_.recurUnaryNeg)).reduceUnaryNeg

  /** Recursively reduces (x-y-z) to (x-(y+z)) on the CompositeTerm tree
    * structure */
  def recurGroupNeg: Term =
    CompositeTerm(f, args map (_.recurGroupNeg)).reduceGroupNeg

  /** Flattens the CompositeTerm tree structure with respect to an associative
    * operator `func` only the top most layer.*/
  def flatten(func: BinOp): Term = {
    def flat(a: List[Term], b: List[Term]): List[Term] = a match {
      case Nil => b
      case CompositeTerm(`func`, l) :: ls => flat(ls, b++l)
      case l :: ls => flat(ls, b++List(l))
    }
    if (func==f) CompositeTerm(f, flat(args, Nil)) else this
  }

  /** Deletes the identity elements `id` with respect to  the operator `func` on
    * the `CompositeTerm(func, _)` tree structure */
  def delId(func: BinOp, id: Term): Term = {
    if (func==f) {
      args filter (_!=id) match {
        case Nil => CompositeTerm(f, List(id))
        case a => CompositeTerm(f, a)
      }
    }
    else this
  }

  /** If CompositeTerm is of the form `CompositeTerm(BinOp(_), a::Nil)` then
    return `a` otherwise the term itself */
  def reduceToSingle: Term = (args, f.op) match {
    case (Nil, _)  => throw new Error("Something is wrong")
    case (_ :: Nil, "+") | (_ :: Nil, "*") => args(0)
    case _ => this
  }

  /** Reduces the mulitiplicy of the same object in the CompositeTerm tree Structure
    * for example
    *
    * `(x+x+x+x)` is converted to `4*x` and `(x*x*x*x)` is converted to `x**4`
    */
  def reduceMultiplicity: Term = f match {
    case BinOp("+") => {
      val a = reducePartial.args.groupBy(identity).map{case (x, ls) => ((Integer(ls.size)*x))}.toList
      CompositeTerm(f, a)
    }
    case BinOp("*") => {
      val a = reducePartial.args.groupBy(identity).map{case (x, ls) => (x**(Integer(ls.size)))}.toList
      CompositeTerm(f, a)
    }
    case _ => this
  }

  /** Reduces the CompositeTerm(BinOp("*"), List(...,Integer(0),...))
    * to Integer(0) */
  def reduceMulZero: Term = f match {
    case BinOp("*") => if (args.exists(_==Integer(0))) Integer(0) else this
    case _ => this
  }

  /** Rule 0: Simplify numbers with respect to corresponding function
    * Rule 1: Keep Simplified Number as first term of list in CompositeTerm(BinOp("*"), _)
    * Rule 2: Keep Simplified Number as  last term of list in CompositeTerm(BinOp("+"), _)
    */
  def reduceNumber: Term = {
    f match {
      case BinOp("*") => {
        val (num, terms) = reducePartial.args partition {
          case _:Number => true;case _ => false }
        def flatNum(l: List[Term], a: Term): Term = l match {
          case Nil => a;
          case x :: xs => flatNum(xs, a*x)
        }
        CompositeTerm(BinOp("*"), flatNum(num, Integer(1)) :: terms)
      }
      case BinOp("+") => {
        val (num, terms) = reducePartial.args partition {
          case _:Number => true;case _ => false }
        def flatNum(l: List[Term], a: Term): Term = l match {
          case Nil => a;
          case x :: xs => flatNum(xs, a+x)
        }
        CompositeTerm(BinOp("+"), terms++List(flatNum(num, Integer(0))))
      }
      case _ => this
    }
  }

  /** Reduces -(-x) to x */
  // This is rather wierd Had -x been represented as (-1)*x
  // Then no need of this function.
  def reduceUnaryNeg: Term = (f, args(0)) match {
    case (UnaryOp("-"), CompositeTerm(UnaryOp("-"), x:: Nil)) => x
    case _ => this
  }

  /** Reduces (x - y - z) to (x - (y+z)) and x*(-y)*(-z) = x*y*z */
  def reduceGroupNeg: Term = f match {
    case BinOp("+") => {
      val (neg, pos) = args partition {
        case CompositeTerm(UnaryOp("-"),_) => true; case _ => false
      }
      val newNeg = neg map {_.args(0)}
      val negList = CompositeTerm(BinOp("+"), newNeg)::Nil
      CompositeTerm(f, pos++List(CompositeTerm(UnaryOp("-"), negList)))
    }
    case BinOp("*") => {
      val (neg, pos) = args partition {
        case CompositeTerm(UnaryOp("-"),_) => true; case _ => false
      }
      val l = neg map {_.args(0)}
      // Assumption that reduceNumber is already done if no number is present
      // by default 1 is assumed.
      val (n, t): (Number, List[Term]) = pos match {
        case x@ ((a@(_: Number)):: Nil) => (a, Nil)
        case x@ ((a@(_: Number)) :: _) => (a, pos.tail)
        case _ => (Integer(1), pos)
      }
      if ((l.length%2 == 0 && n.signum==1)||(l.length % 2==1 && n.signum == -1))
        CompositeTerm(f, List(n.abs) ++ t ++ l)
      else CompositeTerm(UnaryOp("-"), CompositeTerm(f, List(n.abs) ++ t ++ l) :: Nil)
    }
    case _ => this
  }

  // #TODO Write  distributive method which does `a*(b+c)*d` to `a*b*d + a*c*d`
  // This doesn't necessarily depend on Ordering because of it's definition.
  // Even a corresponding recur term

  // #TODO Write a method that does abs(-x) to this (abs(-1)*abs(x))=abs(x)

  // TestExpression.scala tests fail when recurRedMul is included inside.
  // So this exists as a seperate entity

  lazy val reducePartial: Term = {
    val flatAM = recurFlatten(BinOp("+")).recurFlatten(BinOp("*"))
    val delMulId = flatAM.recurDelId(BinOp("*"), Integer(1))
    val delAddId = delMulId.recurDelId(BinOp("+"), Integer(0))
    delAddId.recurSingle.recurMulZero.recurUnaryNeg
  }

  // #TODO May be reduce should be written in a recursive way like
  // reducePartial.(recurRedMul.reducePartial)*n reduce until the
  // CompositeTerm no longer simplifies
  // Changing the order of functions creates error.
  lazy val reduce: Term = reducePartial.recurRedMul.reducePartial.recurRedNum.recurGroupNeg

  /** Differentiate the CompositeTerm with respect to an independent variable `x` */
  // #TODO write cases for operators abs, **, -, /
  // Is reducePartial.args the correct way to do things??
  def diff(x: Symbol): Term = f match {
    case BinOp("+") => CompositeTerm(f, args.map(_.diff(x)))
    case BinOp("*") => {
      val terms = reducePartial.args
      val a = terms.zipWithIndex.map {
        case (_, i) =>
          terms.zipWithIndex.map { case (xs, j) => if (i == j) xs.diff(x) else xs }
      }
      CompositeTerm(BinOp("+"), a.map(CompositeTerm(f, _))).reducePartial.reduceToSingle
    }
    case _ => BinOp("diff")(this, x)
  }

  override def diff(x: Symbol, n: Int): Term = {
    assert(n > 0, throw new Error("n can't less that 1"))
    def diffAcc(t: Term, s: Symbol, m: Int): Term = {
      if (m == 1) t.diff(x)
      else diffAcc(t.diff(x).reducePartial, s, m - 1)
    }
    diffAcc(this, x, n)
  }

  // #TODO write a special Case unary_-
  override def formatToString = f match {
    case UnaryOp(op) => op match {
      case "-" => op + args(0)
      case  _  => op + "("+args(0)+")"
    }
    case BinOp(op)   => (args, op) match {
      case ( Nil,   _) => ""
      case (x::Nil, _) => x.toString
      case (_, "+") => args.zipWithIndex.map{
        case x@ (CompositeTerm(UnaryOp("-"), _), _) =>
          " - " + (x._1.args(0).toString)
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

  // #TODO reducePartial to be replaced with reduce when term ordering rules
  // are written.

  override def toString = reducePartial.formatToString

  /** Experimenting with `==` Not final yet. */
  override def equals(a: Any) = a match {
    case b @ CompositeTerm(_, _) => {
      val c = b.reduce
      c match {
        case that: CompositeTerm =>
          (that.args.toSet == this.reduce.args.toSet) && (this.f == that.f)
        case _ => false
      }
    }
    case _ => false
  }

}

object implicits {
  import scala.language.implicitConversions

  implicit def intToNumber(x: Int) = Integer(x)
  implicit def doubleToNumber(x: Double) = Real(x)
}
