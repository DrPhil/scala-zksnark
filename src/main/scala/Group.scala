
trait Group[T] {
  val zero: T

  def neg(a: T): T

  def plus(a: T, b: T): T
  def minus(a: T, b: T): T = plus(a, neg(b))

  def repeatedSum(b: T, n: BigInt): T = {
    var result = zero
    var base = b
    var divrem = n /% 2

    while (divrem != (0, 0)) {
      if (divrem._2 == 1) {
        result = plus(result, base)
      }

      divrem = divrem._1 /% 2
      base = plus(base, base)
    }

    result
  }
}

object Group {
  implicit class AdditiveSyntax[A](a: A)(implicit g: Group[A]) {
    def + = g.plus(a, _)
    def - = g.minus(a, _)
    def * = g.repeatedSum(a, _)
  }

  implicit class MultiplicativeSyntax[A](a: A)(implicit g: Group[A]) {
    def * = g.plus(a, _)
    def / = g.minus(a, _)
    def ^ = g.repeatedSum(a, _)
  }
}
