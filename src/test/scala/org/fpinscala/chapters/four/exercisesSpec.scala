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

  it should "return the option value when filter applies" in {
    val ob = Some(1)

    filter(ob)(BigInt(_).mod(2) != 0) shouldBe ob
  }
}