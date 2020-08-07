
trait Group[T] {
  val identity: T

  def inverse(a: T): T
  def operation(a: T, b: T): T
  def difference(a: T, b: T): T = operation(a, inverse(b))

  def repeatedOperation(b: T, n: BigInt): T = {
    var result = identity
    var base = b
    var divrem = n /% 2

    while (divrem != (0, 0)) {
      if (divrem._2 == 1) {
        result = operation(result, base)
      }

      divrem = divrem._1 /% 2
      base = operation(base, base)
    }

    result
  }
}

object Group {
  implicit class AdditiveSyntax[A](a: A)(implicit g: Group[A]) {
    def + = g.operation(a, _)
    def - = g.difference(a, _)
    def * = g.repeatedOperation(a, _)
  }

  implicit class MultiplicativeSyntax[A](a: A)(implicit g: Group[A]) {
    def * = g.operation(a, _)
    def / = g.difference(a, _)
    def ^ = g.repeatedOperation(a, _)
  }
}
