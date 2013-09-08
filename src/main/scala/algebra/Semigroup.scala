package prime.algebra

trait Semigroup[A] {
  def plus(l: A, r: A): A
}
