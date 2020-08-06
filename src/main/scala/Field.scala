
case class Field[T](additive: Group[T], multiplicative: Group[T]) {
  val zero = additive.zero
  def plus = additive.plus _
  def minus = additive.minus _
  def neg = additive.neg _

  val one = multiplicative.zero
  def times = multiplicative.plus _
  def div = multiplicative.minus _
  def inv = multiplicative.neg _

  def exp = multiplicative.repeatedSum _
}

object Field {
  implicit class Syntax[A](a: A)(implicit f: Field[A]) {
    def unary_- = f.neg(a)

    //Why does my program not compile if
    //plus is defined as: def + = f.plus(a, _) ?
    def +(that: A) = f.plus(a, that)
    def - = f.minus(a, _)
    def * = f.times(a, _)
    def / = f.div(a, _)
    def ^ = f.exp(a, _)
  }

  implicit class RichInt[A](i: Int)(implicit f: Field[A]) {
    def *(that: A) = f.additive.repeatedSum(that, i)
  }
}
