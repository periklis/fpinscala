package org.fpinscala.chapters.four

import org.fpinscala.chapters.four.Exercises._
import org.fpinscala.chapters.four.Exercises.Option._
import org.scalatest.{FlatSpec, Matchers}

class ExercisesSpec extends FlatSpec with Matchers {

  behavior of "exercise 4.1 - Option"

  it should "return map to None if Option None" in {
    val ob: Option[Int] = None
    map(ob)(_ + 1) shouldBe None
  }

  it should "return mapped option for Some value" in {
    val ob = Some(1)
    map(ob)(_ + 1) shouldBe Some(2)
  }

  it should "return None when flatMap on Option None" in {
    val ob: Option[Int] = None

    flatMap(ob)(a => Some(a + 1)) shouldBe None
  }

  it should "return option of mapped value for Some value" in {
    val ob = Some(1)
    flatMap(ob)(a => Some(a.toString)) shouldBe Some("1")
  }

  it should "return default value when option None" in {
    val ob: Option[Int] = None

    getOrElse(ob)(2) shouldBe 2
  }

  it should "return initial value when option of some value" in {
    val ob = Some(1)

    getOrElse(ob)(2) shouldBe 1
  }

  it should "return other option when option None" in {
    val ob: Option[Int] = None
    val oe              = Some(1)

    orElse(ob)(oe) shouldBe oe
  }

  it should "return intial option when some option value" in {
    val ob = Some(1)
    val oe = Some(2)

    orElse(ob)(oe) shouldBe ob
  }

  it should "return None when option None" in {
    val ob: Option[Int] = None

    filter(ob)(a => true) shouldBe None
  }

  it should "return the option value when filter predicate applies" in {
    val ob = Some(1)

    filter(ob)(BigInt(_).mod(2) != 0) shouldBe ob
  }

  it should "return None when filter predicate does not apply" in {
    val ob = Some(1)

    filter(ob)(BigInt(_).mod(2) == 0) shouldBe None
  }

  behavior of "exericse 4.2"

  it should "return None when empty sequence" in {
    val xs: Seq[Double] = Seq()

    variance(xs) shouldBe None
  }

  it should "return the calculated variance for a sequence of doubles" in {
    val xs = Seq(1.0, 2.0, 3.0, 4.0)

    variance(xs) shouldBe Some(1.25)
  }

  behavior of "exercise 4.3"

  it should "return None when a single option argument is None" in {
    val a: Option[Int] = None
    val b              = Some(2)
    val f              = (a: Int, b: Int) => a + b

    map2(a, b)(f) shouldBe None
  }

  it should "return calculated value when applying f on bowth option arguments" in {
    val a = Some(1)
    val b = Some(2)
    val f = (a: Int, b: Int) => (a + b).toString

    map2(a, b)(f) shouldBe Some("3")
  }
}
