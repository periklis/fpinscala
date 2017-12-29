package org.fpinscala.chapters.six

import org.fpinscala.chapters.six.Exercises._
import org.scalatest.{FlatSpec, Matchers}

class ExercisesSpec extends FlatSpec with Matchers {

  behavior of "exercise 6.1"

  it should "return a non-negative random number when standard rng given" in {
    RNG.nonNegativeNextInt(SimpleRNG(42))._1 shouldBe 16159453
  }

  it should "return a non-negative random number when rng seed produces a negative one" in {
    RNG.nonNegativeNextInt(SimpleRNG(-235200))._1 should be >= 0
  }
}
