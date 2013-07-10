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
  def simplifyTerm: Term
  def groupMultiple: Term
  def mulZero: Term
  def reduceNumber: Term
  def groupNegative: Term
  def minusAbs: Term
  def expandUnaryNeg: Term
  def groupDivide: Term

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
  def simplifyTerm: Term = this

  /** Simply returns the atomic term itself */
  def groupMultiple: Term = this

  /** Simply returns the atomic term itself */
  def mulZero: Term = this

  /** Simply returns the atomic term itself */
  def reduceNumber: Term = this

  /** Simply returns the atomic term itself */
  def groupNegative: Term = this

  /** Simply returns the atomic term itself */
  def minusAbs: Term = this

  /** Simply returns the atomic term itself */
  def groupDivide: Term = this

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
    (f, args map {_.flatten}) match {
      case (BinOp("+"), ar) => {
        CompositeTerm(BinOp("+"), flatList(BinOp("+"), ar, Nil))
      }
      case (BinOp("*"), ar) => {
        CompositeTerm(BinOp("*"), flatList(BinOp("*"), ar, Nil))
      }
      case (_, ar) => CompositeTerm(f, ar)
    }
  }

  /** Deletes the identity elements `id` with respect to  the operator `func` on
    * the `CompositeTerm(func, _)` tree structure */
  def delIdentity: Term = {
    def filterTerm(id: Term, ar: List[Term]): List[Term] = ar filter (_!=id) match {
      case Nil => id :: Nil
      case a => a
    }
    (f, args map {_.delIdentity}) match {
      case (BinOp("+"), ar) => {
        CompositeTerm(BinOp("+"), filterTerm(Integer(0), ar))
      }
      case (BinOp("*"), ar) => {
        CompositeTerm(BinOp("*"), filterTerm(Integer(1), ar))
      }
      case (BinOp("/"), ar) => {
        if (ar(1) == Integer(1)) ar(0) else CompositeTerm(f, ar)
      }
      case (BinOp("**"), ar) => {
        if (ar(1) == Integer(1)) ar(0) else CompositeTerm(f, ar)
      }
      case (UnaryOp("-"), CompositeTerm(UnaryOp("-"), a :: Nil) :: Nil) => a
      case (_, ar) => CompositeTerm(f, ar)
    }
  }

  /** If CompositeTerm is of the form `CompositeTerm(BinOp(_), a::Nil)` then
    return `a` otherwise the term itself */
  def simplifyTerm: Term = (f, args map {_.simplifyTerm}) match {
    case (BinOp("+"), x :: Nil) => x
    case (BinOp("*"), x :: Nil) => x
    case (UnaryOp("-"), CompositeTerm(BinOp("+"), x :: Nil) :: Nil) =>
      CompositeTerm(UnaryOp("-"), x :: Nil)
    case (UnaryOp("-"), a :: Nil) => {
      if (a == Integer(0)) Integer(0) else CompositeTerm(UnaryOp("-"), a :: Nil)
    }
    case (BinOp("/"), List(CompositeTerm(BinOp("/"), a1), CompositeTerm(BinOp("/"), a2))) => {
      CompositeTerm(BinOp("/"), a1(0) * a2(1) :: a1(1) * a2(0) :: Nil)
    }
    case (BinOp("/"), List(CompositeTerm(BinOp("/"), a1), a2)) => {
      CompositeTerm(BinOp("/"), a1(0) :: a2 * a1(1) :: Nil)
    }
    case (BinOp("/"), List(a1, CompositeTerm(BinOp("/"), a2))) => {
      CompositeTerm(BinOp("/"), a1 * a2(1) :: a2(0) :: Nil)
    }
    case (fu, ar) => CompositeTerm(fu, ar)
  }

  /** Reduces the mulitiplicy of the same object in the CompositeTerm tree Structure
    * for example
    *
    * `(x+x+x+x)` is converted to `4*x` and `(x*x*x*x)` is converted to `x**4`
    */
  def groupMultiple: Term = (f, args map {_.groupMultiple}) match {
    case (BinOp("+"), ar) => {
      val finalList = ar.groupBy(identity).map{
        case (x,ls) => {val n = ls.size; if(n==1) x else Integer(n)*x}
      }.toList
      CompositeTerm(BinOp("+"), finalList)
    }
    case (BinOp("*"), ar) => {
      val finalList = ar.groupBy(identity).map{
        case (x,ls) => {val n = ls.size; if(n==1) x else x**Integer(n)}
      }.toList
      CompositeTerm(BinOp("*"), finalList)
    }
    case (fu, ar) => CompositeTerm(fu, ar)
  }

  /** Reduces the CompositeTerm(BinOp("*"), List(..., 0, ...))
    * to Integer(0) or CompositeTerm(BinOp("/"), 0 :: _ :: Nil) to 0  */
  def mulZero: Term = (f, args map {_.mulZero}) match {
    case (BinOp("*"), ar) => {
      if (ar exists (_==Integer(0))) Integer(0) else CompositeTerm(BinOp("*"), ar)
    }
    case (BinOp("/"), ar) => {
      if (ar(0) == Integer(0)) Integer(0) else CompositeTerm(BinOp("/"), ar)
    }
    case (fu, ar) => CompositeTerm(fu, ar)
  }

  /** Rule 0: Simplify numbers with respect to corresponding operator
    * Rule 1: Keep Simplified Number as first term of list in CompositeTerm(BinOp("*"), _)
    * Rule 2: Keep Simplified Number as  last term of list in CompositeTerm(BinOp("+"), _)
    */
  def reduceNumber: Term = (f, args map {_.reduceNumber}) match {
    case (BinOp("*"), ar) => {
      val (num, terms) = ar partition {
        case _:Number => true;case _ => false }
      val number = ((Integer(1): Term) /: num) (_ * _)
      if (number == Integer(1)) CompositeTerm(BinOp("*"), terms)
      else CompositeTerm(BinOp("*"), number :: terms)
    }
    case (BinOp("+"), ar) => {
      val (num, terms) = ar partition {
        case _:Number => true;case _ => false }
      val number = ((Integer(0): Term) /: num) (_ + _)
      terms match {
        case Nil => number
        case _ =>
          if (number == Integer(0)) CompositeTerm(BinOp("+"), terms)
          else CompositeTerm(BinOp("+"), terms++List(number))
      }
    }
    case (fu, ar) => CompositeTerm(fu, ar)
  }

  /** Reduces (x - y - z) to (x - (y+z)) and x*(-y)*(-z) = x*y*z */
  // #TODO Find the culprit that causes " - 0" here and there
  def groupNegative: Term = (f, args map {_.groupNegative}) match {
    case (BinOp("+"), ar) => {
      val (neg, pos) = {
        def cmp(a: List[Term], b: List[CompositeTerm], c: List[Term]): (List[CompositeTerm], List[Term]) = a match {
          case Nil => (b, c)
          case (x @ CompositeTerm(UnaryOp("-"), _)) :: xs => cmp(xs, b ++ List(x), c)
          case x :: xs => cmp(xs, b, c++List(x))
        }
        cmp(ar, Nil, Nil)
      }
      (neg, pos) match {
        case (Nil, _) => CompositeTerm(BinOp("+"), pos)
        case (_, Nil) =>
          CompositeTerm(UnaryOp("-"), CompositeTerm(BinOp("+"), neg map {_.args(0)}) :: Nil)
        case (_, _) => {
          val negTerm = CompositeTerm(BinOp("+"), neg map {_.args(0)})
          CompositeTerm(BinOp("+"), pos ++ List(CompositeTerm(UnaryOp("-"), negTerm :: Nil)))
        }
      }
    }
    case (BinOp("*"), ar) => {
      val (neg, pos) = {
        def cmp(a: List[Term], b: List[CompositeTerm], c: List[Term]): (List[CompositeTerm], List[Term]) = a match {
          case Nil => (b, c)
          case (x @ CompositeTerm(UnaryOp("-"), _)) :: xs => cmp(xs, b ++ List(x), c)
          case x :: xs => cmp(xs, b, c++List(x))
        }
        cmp(ar, Nil, Nil)
      }
      val l = neg map {_.args(0)}
      // Assumption that reduceNumber is already done if no number is present
      // by default 1 is assumed.
      val (n, t): (Number, List[Term]) = pos match {
        case (a: Number) :: _ => (a, pos.tail)
        case _ => (Integer(1), pos)
      }
      if ((l.length%2 == 0 & n.signum==1) | (l.length % 2==1 & n.signum == -1)) {
        if (n.abs == Integer(1)) CompositeTerm(f, t ++ l)
        else CompositeTerm(f, List(n.abs) ++ t ++ l)
      }
      else {
        if (n.abs == Integer(1)) CompositeTerm(UnaryOp("-"), CompositeTerm(f, t ++ l)::Nil)
        else CompositeTerm(UnaryOp("-"), CompositeTerm(f, List(n.abs) ++ t ++ l)::Nil)
      }
    }
    case (_, ar) => CompositeTerm(f, ar)
  }

  // #TODO Write  distributive method which does `a*(b+c)*d` to `a*b*d + a*c*d`
  // This doesn't necessarily depend on Ordering because of it's definition.
  // Even a corresponding recur term

  /** Reduces expresstion like this abs(-x) to abs(x) */
  def minusAbs: Term = (f, args map {_.minusAbs}) match {
    case (UnaryOp("abs"), CompositeTerm(UnaryOp("-"), a::Nil)::Nil) => a
    case (_, ar) => CompositeTerm(f, ar)
  }

  def groupDivide: Term = (f, args map {_.groupDivide}) match {
    case (BinOp("*"), ar) => {
      if (ar exists {case CompositeTerm(BinOp("/"), _)=>true; case _=>false}) {
        val (div, rest) = {
          def cmp(a: List[Term], b: List[CompositeTerm], c: List[Term]): (List[CompositeTerm], List[Term]) = a match {
            case Nil => (b, c)
            case (x @ CompositeTerm(BinOp("/"), _)) :: xs => cmp(xs, b ++ List(x), c)
            case x :: xs => cmp(xs, b, c++List(x))
          }
          cmp(ar, Nil, Nil)
        }
        def multdiv(x: CompositeTerm, y: CompositeTerm): CompositeTerm =
          CompositeTerm(BinOp("/"), x.args(0)*y.args(0) :: x.args(1)*y.args(1) :: Nil)
        val one = CompositeTerm(BinOp("/"), List(Integer(1), Integer(1)))
        val divTerm = (one /: div) (multdiv(_, _))
        val num = (divTerm.args(0) /: rest) (_ * _)
        val dnum = divTerm.args(1)
        CompositeTerm(BinOp("/"), num :: dnum :: Nil)
      }
      else CompositeTerm(f, ar)
    }
    case (_, ar) => CompositeTerm(f, ar)
  }


  lazy val reduce: Term = {
    def reduceOne(prev: Term, after: Term): Term =
      if (prev == after) after
      else {
        val next1 = after.flatten.delIdentity.reduceNumber.simplifyTerm.mulZero.cancel
        val next = next1.groupMultiple.groupNegative.groupDivide
        reduceOne(after, next)
      }
    reduceOne(Integer(0), this)
  }

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

  def cancel: Term = (f, args map {_.cancel}) match {
    case (BinOp("+"), ar) => {
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
      CompositeTerm(BinOp("+"), cancelAdd(ar, Nil))
    }
    case (BinOp("/"), List(CompositeTerm(BinOp("*"), a1), CompositeTerm(BinOp("*"), a2))) => {
      def cancelDiv(n: List[Term], d: List[Term], num: List[Term]): Term = (n, d) match {
        case (Nil, Nil) => num match {
          case Nil => Integer(1)
          case x :: Nil => x
          case _ => CompositeTerm(BinOp("*"), num)
        }
        case (Nil, _) => {
          val ctD = CompositeTerm(BinOp("*"), d)
          num match {
            case Nil => CompositeTerm(BinOp("/"), Integer(1) :: ctD:: Nil)
            case _ => CompositeTerm(BinOp("/"), CompositeTerm(BinOp("*"), num) :: ctD :: Nil)
          }
        }
        case (_, Nil) => num match {
          case Nil => CompositeTerm(BinOp("*"), n)
          case _ => CompositeTerm(BinOp("*"), num ++ n)
        }
        case (_, _) => {
          if (d exists {x => if(x == n.head) true else false}) {
            val newD = (d.toBuffer -= n.head).toList
            cancelDiv(n.tail, newD, num)
          }
          else cancelDiv(n.tail, d, num ++ List(n.head))
        }
      }
      cancelDiv(a1, a2, Nil)
    }
    case (BinOp("/"), ar@ List(a1, CompositeTerm(BinOp("*"), a2))) => {
      if (a2 exists {_ == a1}) {
        (a2.toBuffer -= a1).toList match {
          case Nil => Integer(1)
          case x :: Nil => Integer(1)/ x
          case xs => Integer(1) / CompositeTerm(BinOp("*"), xs)
        }
      }
      else CompositeTerm(BinOp("/"), ar)
    }
    case (BinOp("/"), ar@ List(CompositeTerm(BinOp("*"), a1), a2)) => {
      if (a1 exists {_ == a2}) {
        (a1.toBuffer -= a2).toList match {
          case Nil => Integer(1)
          case x :: Nil => x
          case xs => Integer(1) / CompositeTerm(BinOp("*"), xs)
        }
      }
      else CompositeTerm(BinOp("/"), ar)
    }
    case (BinOp("/"), a1 :: a2 :: Nil) => {
      if (a1 == a2) Integer(1)
      else CompositeTerm(BinOp("/"), a1 :: a2 :: Nil)
    }
    case (_, ar) => CompositeTerm(f, ar)
  }


  def subtract: Term = this

  def expandUnaryNeg: Term = (f, args map {_.expandUnaryNeg}) match {
    case (UnaryOp("-"), CompositeTerm(BinOp("+"), x):: Nil) => {
      CompositeTerm(BinOp("+"), x map {y => CompositeTerm(UnaryOp("-"), List(y))})
    }
    case (_, ar) => CompositeTerm(f, ar)
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

  override def toString = formatToString

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
