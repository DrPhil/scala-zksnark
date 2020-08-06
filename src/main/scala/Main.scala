import Field.Syntax
import Group.AdditiveSyntax

object Main extends App {
  {
    val field = Fₚ(257)
    import field._

    val simpleCurve = EllipticCurve[ℤ](-1, 1)
    import simpleCurve._

    val p1 = Point(0, 1)
    val p2 = Point(3, 5)

    //I've imported the implicit AdditiveSyntax, why do I need to
    //call it explicitly? Shouldn't it be implicitly converted?
    //val p3: Point = p1 + p2
    val p3: Point = AdditiveSyntax(p1) + p2
    println(s"On the curve $simpleCurve, the sum $p1 + $p2 equals $p3.")

    val p4: Point = p1 * 12
    println(s"We can multiply points with integers: $p4")
  }

  {
    val p256 = "115792089210356248762697446949407573530086143415290314195533631308867097853951"
    val field = Fₚ(BigInt(p256))
    import field._

    val P256 = EllipticCurve[ℤ](-3, BigInt("41058363725152142129326129780047268409114441015993725554835256314039467401291"))
    import P256._

    //Official generator from NIST on P-256
    val g = Point(
      BigInt("48439561293906451759052585252797914202762949526041747995844080717082404635286"),
      BigInt("36134250956749795798585127919587881956611106672985015071877198253568414405109")
    )

    println("\nLet's simulate Diffie-Hellman!\n")
    val kₐ = BigInt("31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679")
    val Pₐ = g * kₐ
    println(s"Alice sends Pₐ = $Pₐ")

    val kₑ = BigInt("27182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274")
    val Pₑ = g * kₑ
    println(s"Eric sends Pₑ = $Pₑ")

    val S = Pₑ * kₐ
    println(s"The shared secret is $S")
    assert(S == Pₐ * kₑ)
  }
}
