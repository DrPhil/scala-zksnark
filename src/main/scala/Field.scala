
case class Field[T](additive: Group[T], multiplicative: Group[T]) {
  // It doesn't make sense, but I need identity to be lazy??
  lazy val zero = additive.identity
  def plus = additive.operation _
  def minus = additive.difference _
  def neg = additive.inverse _

  // It doesn't make sense, but I need identity to be lazy??
  lazy val one = multiplicative.identity
  def times = multiplicative.operation _
  def div = multiplicative.difference _
  def inv = multiplicative.inverse _

  def exp = multiplicative.repeatedOperation _
}

object Field {
  implicit class Syntax[A](a: A)(implicit f: Field[A]) {
    def unary_- = f.neg(a)

    def + = f.plus(a, _)
    def - = f.minus(a, _)
    def * = f.times(a, _)
    def / = f.div(a, _)
    def ^ = f.exp(a, _)
  }

  implicit class RichInt[A](i: Int)(implicit f: Field[A]) {
    def *(that: A) = f.additive.repeatedOperation(that, i)
  }

  def zero[T](implicit field: Field[T]) = field.zero
  def one[T](implicit field: Field[T]) = field.one
}
