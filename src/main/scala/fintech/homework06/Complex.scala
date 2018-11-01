package fintech.homework06

final case class Complex(real: Double, imaginary: Double = 0) {

  def +(complex: Complex): Complex =
    Complex(real + complex.real, imaginary + complex.imaginary)

  def +(d: Double): Complex = Complex(real + d, imaginary)

  def -(complex: Complex) =
    Complex(real - complex.real, imaginary - complex.imaginary)

  def *(complex: Complex): Complex =
    Complex(real * complex.real - imaginary * complex.imaginary, real * complex.imaginary + complex.real * imaginary)

  def /(complex: Complex): Complex = {
    if (complex == Complex.zero)
      throw new ArithmeticException("denominator = 0")
    val (a, b, c, d) = (real, imaginary, complex.real, complex.imaginary)
    Complex((a * c + b * d) / (c * c + d * d), (b * c - a * d) / (c * c + d * d))
  }

  def compare(c: Complex): Int =
    if ((real compare c.real) == 0)
      imaginary compare c.imaginary
    else
      real compare c.real
}

object Complex {
  val zero = Complex(0)
  val one = Complex(1)

  implicit def toComplex(value: Double): Complex = Complex(value)
  implicit class ForI(private val value: Double) extends AnyVal {
    def i: Complex = Complex(0, value)
  }
  val i = Complex(0, 1)

  implicit val complexNumeric: Numeric[Complex] = new Numeric[Complex] {
    override def plus(x: Complex, y: Complex): Complex = x + y
    override def minus(x: Complex, y: Complex): Complex = x - y
    override def times(x: Complex, y: Complex): Complex = x * y
    override def negate(x: Complex): Complex = zero - x
    override def fromInt(x: Int): Complex = Complex(x)
    override def toInt(x: Complex): Int = x.real.toInt
    override def toLong(x: Complex): Long = x.real.toLong
    override def toFloat(x: Complex): Float = x.real.toFloat
    override def toDouble(x: Complex): Double = x.real
    override def compare(x: Complex, y: Complex): Int = x compare y
  }

  implicit val complexEq: Eq[Complex] = _ == _
}
