package prime.algebra

trait Field[A] extends Ring[A] {

  def inverse(a: A): A = {
    assertNonZero(a)
    div(one, a)
  }

  def div(l: A, r: A): A = {
    assertNonZero(r)
    times(l, inverse(r))
  }
}
