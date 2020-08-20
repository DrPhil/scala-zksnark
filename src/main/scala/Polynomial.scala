import Field._

case class Polynomial[T : Field] private (vs: Seq[T]) {
  import Polynomial.isAdditiveGroup
  import Group.AdditiveSyntax

  lazy val cache: Seq[Polynomial[T]] = for (i <- 0 to 2*degree) yield Polynomial.monomial(one, i).divrem(this)._2

  val degree = vs.length - 1
  lazy val leadingCoefficient: T = apply(degree)

  def apply(n: Int): T = vs.applyOrElse(
    degree - n,
    { _: Int => zero }
  )

  def timesConstant(b: T) = Polynomial(vs.map(_ * b))
  def timesX(n: Int) = new Polynomial(vs ++ Seq.fill(n)(zero))

  def *(b: Polynomial[T]) = {
    var result = isAdditiveGroup.identity

    for {
      i <- 0 to degree
      x = apply(i)
    } {
      result += new Polynomial(b.timesConstant(x).vs ++ Seq.fill(i)(zero))
    }

    Polynomial(result.vs)
  }

  def divrem(divisor: Polynomial[T]): (Polynomial[T], Polynomial[T]) = {
    var quotient: Polynomial[T] = isAdditiveGroup.identity
    var remainder: Polynomial[T] = this

    while (remainder.degree >= divisor.degree) {
      val coefficient: T = remainder.leadingCoefficient / divisor.leadingCoefficient
      val m = Polynomial.monomial(coefficient, remainder.degree - divisor.degree)

      quotient += m
      remainder -= m * divisor
    }

    (quotient, remainder)
  }

  def fastrem(divisor: Polynomial[T]): Polynomial[T] = {

    var remainder = this

    while (remainder.degree >= divisor.degree) {
      remainder += divisor.cache(remainder.degree).timesConstant(remainder.leadingCoefficient)
      remainder = Polynomial(remainder.vs.tail)
    }

    remainder
  }

  override def toString: String = {
    if (vs == Seq.empty) {
      return zero.toString
    }

    val exps = "⁰¹²³⁴⁵⁶⁷⁸⁹".split("")
    def exp(i: Int) = {
      if (degree - i == 0) {
        ""
      } else if (degree - i == 1) {
        "x"
      } else {
        s"x${(degree - i).toString.map(x => exps(x.asDigit)).mkString}"
      }
    }

    vs.zipWithIndex.filter {
      case (x, _) => x != zero
    }.map {
      case (x, i) =>
        val xstr = if (x == one && i != degree) "" else x.toString
        s"$xstr${exp(i)}"
    }.mkString(" + ")
  }
}

object Polynomial {
  def apply[T : Field](vs: Seq[T]): Polynomial[T] = {
    if (vs.headOption.contains(zero)) {
      return Polynomial(vs.tail)
    }

    new Polynomial(vs)
  }

  implicit def isAdditiveGroup[T : Field] = new Group[Polynomial[T]] {
    def inverse(a: Polynomial[T]) = Polynomial(a.vs.map(- _))
    def operation(a: Polynomial[T], b: Polynomial[T]) = {
      val maxDegree = scala.math.max(a.degree, b.degree)
      Polynomial(for (i <- maxDegree to 0 by -1) yield a(i) + b(i))
    }

    val identity = Polynomial(Seq.empty)
  }

  def monomial[T : Field](coefficient: T, degree: Int): Polynomial[T] =
    Polynomial(coefficient +: Seq.fill(degree)(zero))
}
