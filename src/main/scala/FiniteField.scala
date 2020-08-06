import scala.math.BigInt
import scala.language.implicitConversions

case class Fₚ(mod: BigInt) {
  require(mod.isProbablePrime(10), "The field is not a primitive prime field!")

  case class ℤ private (v: BigInt)

  object ℤ{
    implicit def apply(v: BigInt): ℤ = new ℤ(v.mod(mod))
  }

  implicit def bigIntToℤ(v: BigInt): ℤ = ℤ(v)
  implicit def intToℤ(v: Int): ℤ = ℤ(v)

  val isAdditiveGroup = new Group[ℤ] {
    def neg(a: ℤ) = ℤ(-a.v)
    def plus(a: ℤ, b: ℤ) = ℤ(a.v + b.v)

    val zero = 0
  }

  val isMultiplicativeGroup = new Group[ℤ] {
    def neg(a: ℤ) = ℤ(a.v.modInverse(mod))
    def plus(a: ℤ, b: ℤ) = ℤ(a.v * b.v)

    val zero = 1
  }

  implicit val isField = Field(isAdditiveGroup, isMultiplicativeGroup)
}
