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

  behavior of "exercise 6.2"

  it should "return a double in range [0, 1)" in {
    RNG.double(SimpleRNG(0))._1 should (be >= 0.0 and be < 1.0)
  }

  behavior of "exercise 6.3"

  it should "return a tuple of (Int, Double)" in {
    RNG.intDouble(SimpleRNG(0))._1 shouldBe a[(Int, Double)]
  }

  it should "return a tuple of (Double, Int)" in {
    RNG.doubleInt(SimpleRNG(0))._1 shouldBe a[(Double, Int)]
  }

  it should "return a tuple of (Double, Double, Double)" in {
    RNG.double3(SimpleRNG(0))._1 shouldBe a[(Double, Double, Double)]
  }

  behavior of "exercise 6.4"

  it should "return the empty list when count zero given" in {
    RNG.ints(0)(SimpleRNG(0))._1 shouldBe Nil
  }

  it should "return a list with n random ints when count n given" in {
    RNG.ints(2)(SimpleRNG(0))._1 should have length 2
  }
}
