package prime.algebra

trait Monoid[A] extends Semigroup[A]  {

  def zero: A

  def isNonZero(v: A): Boolean = (v != zero)

  def assertNonZero(v : A) {
    if(!isNonZero(v)) {
      throw new java.lang.IllegalArgumentException("argument should not be zero")
    }
  }
}
