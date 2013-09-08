package prime.algebra

trait Group[A] extends Monoid[A] {

  def neg(a: A): A = minus(zero, a)

  def minus(a: A, b: A) = plus(a, neg(b))
}
