object Bls12 {
  val p = BigInt("4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787")
  val q = BigInt("52435875175126190479447740508185965837690552500527637822603658699938581184513")

  val field = Fₚ(p)
  import field._
  import Field._

  val f2 = field.Fₚⁿ(1, 0, 1)
  val f12 = field.Fₚⁿ(1, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, 0, 2)
  import f2._

  val b = EllipticCurve[ℤ](0, 4)
  val b2 = EllipticCurve[Z](Z(0), Z(4, 4))
  val b12 = EllipticCurve[f12.Z](f12.Z(0), f12.Z(4))

  val twister = f12.Z(1, 0)
  def twist(point: b2.Point): b12.Point = point match {
    case b2.Infinity => b12.Infinity
    case b2.Point(x, y) =>
      val f12_x = f12.Z(x(1), 0, 0, 0, 0, 0, x(0) - x(1))
      val f12_y = f12.Z(y(1), 0, 0, 0, 0, 0, y(0) - y(1))

      return b12.Point(f12_x / (twister * twister), f12_y / (twister * twister * twister))
  }

  val g = b.Point(
    BigInt("3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507"),
    BigInt("1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569")
  )

  val g2 = b2.Point(
    Z(
      BigInt("3059144344244213709971259814753781636986470325476647558659373206291635324768958432433509563104347017837885763365758"),
      BigInt("352701069587466618187139116011060144890029952792775240219908644239793785735715026873347600343865175952761926303160")
    ),
    Z(
      BigInt("927553665492332455747201965776037880757740193453592970025027978793976877002675564980949289727957565575433344219582"),
      BigInt("1985150602287291935568054521177171638300868978215655730859378665066344726373823718423869104263333984641494340347905")
    )
  )

  import f12.Z._

  def ℓ(p: b12.Point, q: b12.Point, t: b12.Point): f12.Z = (p, q, t) match {
    case (b12.Point(xᵢ, yᵢ), b12.Point(xⱼ, yⱼ), b12.Point(xₜ, yₜ)) =>
      if (xᵢ != xⱼ) {
        val m = (yⱼ - yᵢ) / (xⱼ - xᵢ)
        return m * (xₜ - xᵢ) - (yₜ - yᵢ)
      } else if (yᵢ == yⱼ) {
        val m = 3 * xᵢ * xᵢ / (2 * yᵢ)
        return m * (xₜ - xᵢ) - (yₜ - yᵢ)
      } else {
        return xₜ - xᵢ
      }
  }

  val ate_loop_count = BigInt("15132376222941642752")
  val log_ate_loop_count = 62
  def millerLoop(q: b12.Point, p: b12.Point): f12.Z = {
    if (q == b12.Infinity) {
      return one
    }

    import Group.AdditiveSyntax
    import b12._

    var r = q
    var f = one

    for (i <- log_ate_loop_count to 0 by -1) {
      f = f * f * ℓ(r, r, p)
      r = r * 2

      if (ate_loop_count.testBit(i)) {
        f = f * ℓ(r, q, p)
        r = r + q
      }
    }

    f
  }

  val exponent = (p.pow(12) - 1) / q
  def pairing(p: b.Point, q: b2.Point): f12.Z = {
    val newp: b12.Point = p match {
      case b.Infinity => return one
      case b.Point(x, y) => b12.Point(f12.Z(x), f12.Z(y))
    }

    val millerResult = millerLoop(twist(q), newp)

    millerResult ^ exponent
  }
}
