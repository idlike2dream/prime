package prime.algebra

trait Ring[A] extends Group[A] {

  def one: A

  def times(l: A, r: A): A
}
