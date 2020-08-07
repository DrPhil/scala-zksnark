import Field._

case class EllipticCurve[T : Field](a: T, b: T) {
  require(4*(a^3) != 27*(b^2), "Curve is not non-singular")

  sealed trait Point
  case class FinitePoint(x: T, y: T) extends Point {
    override def productPrefix = "Point"
  }
  case object Infinity extends Point

  def isOnCurve(p: Point): Boolean = p match {
    case Infinity => true
    case FinitePoint(x, y) =>
      (y^2) == (x^3) + x*a + b
  }

  object Point {
    def apply(x: T, y: T): Point = FinitePoint(x, y)
    def unapply(p: Point): Option[(T, T)] = p match {
      case Infinity => None
      case FinitePoint(x, y) => Some(x, y)
    }
  }

  implicit def isGroup = new Group[Point] {
    val identity = Infinity

    def inverse(p: Point) = p match {
      case Infinity => Infinity
      case Point(x, y) => Point(x, -y)
    }

    def operation(j: Point, k: Point) = (j, k) match {
      case (j, Infinity) => j
      case (Infinity, k) => k
      case (j, k) if j == inverse(k) => Infinity

      case (j@Point(xⱼ, yⱼ), k) if j == k  =>
        val s  = (3*(xⱼ^2) + a) / (2 * yⱼ)
        val xᵣ = (s^2) - 2*xⱼ
        val yᵣ = yⱼ + s*(xᵣ - xⱼ)

        Point(xᵣ, -yᵣ)

      case (Point(xⱼ, yⱼ), Point(xₖ, yₖ)) =>
        val s  = (yⱼ - yₖ) / (xⱼ - xₖ)
        val xᵣ = (s^2) - xⱼ - xₖ
        val yᵣ = yⱼ + s*(xᵣ - xⱼ)

        Point(xᵣ, -yᵣ)
    }
  }
}
