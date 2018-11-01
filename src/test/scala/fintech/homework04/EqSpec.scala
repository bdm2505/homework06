package fintech.homework04
import org.scalatest.{FlatSpec, Matchers}
import fintech.homework06.{Eq, EqOps, Complex}

class EqSpec extends FlatSpec with Matchers {
  import EqOps._

  it should "compare numbers" in {
    11 =~= 11 shouldBe true
    12.0 =~= 13.0 shouldBe false
  }

  it should "compare numbers with accuracy" in {
    implicit val accDouble: Eq[Double] = accuracy(0.01)
    implicit val accInt: Eq[Int] = accuracy(1)

    10.01.=~=(10.0)(accDouble) shouldBe true
    10.1 =~= 10.0 shouldBe false
    1001 =~= 1000 shouldBe true
    1002 =~= 1000 shouldBe false
  }

  it should "compare seq" in {
    val s1 = Seq(1, 2, 3)
    val s2 = Seq(1, 2, 3)
    val s3 = Seq(1, 2, 3, 4)
    val s4 = Seq(1, 2, 4)

    s1 =~= s2 shouldEqual true
    s1 =~= s3 shouldEqual false
    s1 =~= s4 shouldEqual false
  }

  it should "compare map" in {
    val m1 = Map(12 -> "a")
    val m2 = Map(12 -> "a")
    val m3 = Map(12 -> "b")
    val m4 = Map(12 -> "a", 14 -> "a")

    m1 =~= m2 shouldBe true
    m1 =~= m3 shouldBe false
    m1 =~= m4 shouldBe false
  }

  it should "compare option" in {
    val o1: Option[Int] = Some(1)
    val o2: Option[Int] = Some(2)
    val o3: Option[Int] = None

    o1 =~= o1 shouldBe true
    o1 =~= o2 shouldBe false
    o1 =~= o3 shouldBe false
    o3 =~= o3 shouldBe true
  }

  it should "compare any types" in {
    1 =~= 3.4 shouldBe false
    1 =~= 1.0 shouldBe true

    val s1: Seq[Int] = Seq(1, 4, 2)
    val s2: Seq[Double] = Seq(1.0, 4.0, 2.0)
    val s3: Seq[Any] = Seq(1, 4.0, 2)
    val s4: Seq[String] = Seq("1", "4", "2")

    s1 =~= s2 shouldBe true
    s1 =~= s3 shouldBe true
    s1 =~= s4 shouldBe false
  }

  it should "compare Complex number" in {
    import Complex.ForI
    1 + 4.i =~= 1 + 4.i shouldBe true
  }

  it should "compare Complex number with accuracy" in {
    import Complex.ForI
    implicit val acc: Eq[Complex] = accuracy(Complex(0.2))

    1 + 4.i =~= 1.1 + 4.i shouldBe true
    1 + 4.i =~= 1 + 4.1.i shouldBe true
    1.3 + 3.i =~= 1.3 shouldBe false
  }
}
