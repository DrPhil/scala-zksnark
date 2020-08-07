import scala.math.BigInt
import scala.language.implicitConversions

case class Fₚ(mod: BigInt) {
  require(mod.isProbablePrime(10), "The field is not a primitive prime field!")

  case class ℤ private (v: BigInt) {
    override def toString = v.toString
  }

  object ℤ{
    implicit def apply(v: BigInt): ℤ = new ℤ(v.mod(mod))
    implicit def intToℤ(v: Int): ℤ = ℤ(v)

    val isAdditiveGroup = new Group[ℤ] {
      def inverse(a: ℤ) = ℤ(-a.v)
      def operation(a: ℤ, b: ℤ) = ℤ(a.v + b.v)

      lazy val identity = 0
    }

    val isMultiplicativeGroup = new Group[ℤ] {
      def inverse(a: ℤ) = ℤ(a.v.modInverse(mod))
      def operation(a: ℤ, b: ℤ) = ℤ(a.v * b.v)

      lazy val identity = 1
    }

    implicit val isField = Field(isAdditiveGroup, isMultiplicativeGroup)
  }

  // ----------
  // Extensions
  // ----------
  case class Fₚⁿ(quotient: Polynomial[ℤ]) {
    require(
      quotient.leadingCoefficient == ℤ(1),
      "Polynomial must have 1 as leading coefficient!"
    )

    case class Z private (poly: Polynomial[ℤ])

    object Z {
      import ℤ.isField

      def apply(poly: Polynomial[ℤ]) = new Z(poly.divrem(quotient)._2)
      def apply(zs: ℤ*): Z = apply(Polynomial(zs))

      val isAdditiveGroup = new Group[Z] {
        // It doesn't make sense, but I need identity to be lazy??
        lazy val identity = Z()
        def inverse(a: Z) = Z(Polynomial.isAdditiveGroup.inverse(a.poly))
        def operation(a: Z, b: Z) = Z(Polynomial.isAdditiveGroup.operation(a.poly, b.poly))
      }

      val isMultiplicativeGroup = new Group[Z] {
        // It doesn't make sense, but I need identity to be lazy??
        lazy val identity = Z(1)
        def operation(a: Z, b: Z) = Z(a.poly * b.poly)
        def inverse(a: Z) = {
          import Field._
          import Polynomial.isAdditiveGroup
          import Group.AdditiveSyntax

          var t: (Polynomial[ℤ], Polynomial[ℤ]) =
            (Polynomial(Seq.empty), Polynomial(Seq(1)))
          var r: (Polynomial[ℤ], Polynomial[ℤ]) = (quotient, a.poly)

          while (r._2 != Polynomial(Seq.empty)) {
            val q = r._1.divrem(r._2)._1

            r = (r._2, r._1 - q * r._2)
            t = (t._2, t._1 - q * t._2)
          }

          assert(r._1.degree == 0, "Quotient not irreducible!")

          Z(t._1.timesConstant(ℤ(1) / r._1.leadingCoefficient))
        }
      }

      implicit val ZisField = Field(isAdditiveGroup, isMultiplicativeGroup)
    }
  }

  object Fₚⁿ {
    def apply(zs: ℤ*): Fₚⁿ = apply(Polynomial(zs))
  }
}
