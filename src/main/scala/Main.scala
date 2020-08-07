object Main extends App {
  {
    //rfc2409, section 6.2
    val bigPrime = BigInt("179769313486231590770839156793787453197860296048756011706444423684197180216158519368947833795864925541502180565485980503646440548199239100050792877003355816639229553136239076508735759914822574862575007425302077447712589550957937778424442426617334727629299387668709205606050270810842907692932019128194467627007")
    val field = Fₚ(bigPrime)
    import field._
    import Field._

    println("Diffie-Hellman with modular exponentiation")
    val g: ℤ = 2

    val kₐ = BigInt("31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679")
    val Pₐ = g ^ kₐ
    println(s"Alice sends Pₐ = $Pₐ")

    val kₑ  = BigInt("27182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274")
    val Pₑ = g ^ kₑ
    println(s"Eric sends Pₑ = $Pₑ")

    val S = Pₑ ^ kₐ
    println(s"The shared secret is $S\n")
    assert(S == (Pₐ ^ kₑ))
  }

  {
    val p256 = "115792089210356248762697446949407573530086143415290314195533631308867097853951"
    val field = Fₚ(BigInt(p256))
    import field._
    import Group.AdditiveSyntax

    val P256 = EllipticCurve[ℤ](-3, BigInt("41058363725152142129326129780047268409114441015993725554835256314039467401291"))
    import P256._

    //Official generator from NIST on P-256
    val g = Point(
      BigInt("48439561293906451759052585252797914202762949526041747995844080717082404635286"),
      BigInt("36134250956749795798585127919587881956611106672985015071877198253568414405109")
    )

    println("Diffie-Hellman with Elliptic curves")
    val kₐ = BigInt("31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679")
    val Pₐ = g * kₐ
    println(s"Alice sends Pₐ = $Pₐ")

    val kₑ = BigInt("27182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274")
    val Pₑ = g * kₑ
    println(s"Eric sends Pₑ = $Pₑ")

    val S = Pₑ * kₐ
    println(s"The shared secret is $S\n")
    assert(S == Pₐ * kₑ)
  }

  {
    val field = Fₚ(257)
    import field._

    val p = Polynomial[ℤ](Seq(1, -2, 0, -4))
    val d = Polynomial[ℤ](Seq(1, -3))

    import Group.AdditiveSyntax
    import Polynomial._

    val (q, r) = p.divrem(d)
    println("We can do polynomial division on arbitrary polynomials")
    println(s"$p = ($q)($d) + $r\n")
    assert(p == q*d + r)
  }

  {
    val field = Fₚ(2)
    import field._

    // d = x⁸ + x⁴ + x³ + x + 1
    val extendedField = Fₚⁿ(1, 0, 0, 0, 1, 1, 0, 1, 1)
    import extendedField._

    import Field._
    import Z._

    // y = x⁶ + x⁴ + x + 1
    val y = Z(1, 0, 1, 0, 0, 1, 1)

    val inv = one / y
    println("In a simply extended finite field every polynomial has an inverse.")
    println(s"$y = $one / $inv")
    assert(y * inv == one)
  }
}
