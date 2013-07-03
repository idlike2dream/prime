package prime.core

object Simplify {

  // expand((x+y)*(x+y)) => (x**2 + 2*x*y + y**2)
  def expand(x: Term): Term = {

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
      case _ => y
    }

    def expandRecur(y: Term): Term = y match {
      case CompositeTerm(BinOp("*"), a) =>
        expandHere(CompositeTerm(BinOp("*"), a map {expandRecur(_)} ))
      case _ => y
    }

    expandRecur(x)
  }

}
