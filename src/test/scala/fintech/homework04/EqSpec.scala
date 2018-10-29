package fintech.homework04
import org.scalatest.{FlatSpec, Matchers}
import fintech.homework06.{Eq, EqOps}

class EqSpec extends FlatSpec with Matchers {
  import Eq._
  it should "compare numbers" in {
    import EqOps.{int, double}
    11 =~= 11 shouldBe true
    12.0 =~= 13.0 shouldBe false
  }

  it should "compare numbers with accuracy" in {
    implicit val accDouble: Eq[Double] = accuracy(0.01)
    implicit val accInt: Eq[Int] = accuracy(1)

    10.01 =~= 10.0 shouldBe true
    10.1 =~= 10.0 shouldBe false
    1001 =~= 1000 shouldBe true
    1002 =~= 1000 shouldBe false
  }

  it should "compare seq" in {
    import EqOps.{int, seq}

    val s1 = Seq(1, 2, 3)
    val s2 = Seq(1, 2, 3)
    val s3 = Seq(1, 2, 3, 4)
    val s4 = Seq(1, 2, 4)

    s1 =~= s2 shouldEqual true
    s1 =~= s3 shouldEqual false
    s1 =~= s4 shouldEqual false
  }

  it should "compare map" in {
    import EqOps.{int, string, map}

    val m1 = Map(12 -> "a")
    val m2 = Map(12 -> "a")
    val m3 = Map(12 -> "b")
    val m4 = Map(12 -> "a", 14 -> "a")

    m1 =~= m2 shouldBe true
    m1 =~= m3 shouldBe false
    m1 =~= m4 shouldBe false
  }

  it should "compare option" in {
    import EqOps.{option, int}

    val o1: Option[Int] = Some(1)
    val o2: Option[Int] = Some(2)
    val o3: Option[Int] = None

    o1 =~= o1 shouldBe true
    o1 =~= o2 shouldBe false
    o1 =~= o3 shouldBe false
    o3 =~= o3 shouldBe true

  }
}
