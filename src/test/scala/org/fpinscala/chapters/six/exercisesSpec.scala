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

  behavior of "exercise 6.5"

  it should "return a double in range [0, 1)" in {
    RNG.WithRand.double(SimpleRNG(0))._1 should (be >= 0.0 and be < 1.0)
  }

  behavior of "exercise 6.6"

  it should "return the combination of two random generators" in {
    val r = RNG.map2(RNG.nonNegativeNextInt, RNG.WithRand.double)((_, _))

    r(SimpleRNG(0))._1 shouldBe a[(Int, Double)]
  }

  behavior of "exercise 6.7"

  it should "return a list with n random ints when count n given" in {
    val rngs = List.fill(3)(RNG.nonNegativeNextInt)

    RNG.sequence(rngs)(SimpleRNG(0))._1 should have length 3
  }

  behavior of "exercise 6.8"

  it should "return a random not less than 16159454" in {
    RNG.nonNegativeLessThan(16159454)(SimpleRNG(42))._1 shouldBe 16159453
  }

  it should "return a random not less than n" in {
    RNG.nonNegativeLessThan(2)(SimpleRNG(42))._1 shouldBe 1
  }

  behavior of "exercise 6.9"

  it should "return the mapped random int when f applied" in {
    RNG.WithFlatMap.map(RNG.nonNegativeNextInt)(_ + 1)(SimpleRNG(0))._1 shouldBe 1
  }

  it should "return the combination of both random ints when f applied" in {
    val g = RNG.nonNegativeNextInt

    RNG.WithFlatMap.map2(g, g)(_ + _ + 2)(SimpleRNG(0))._1 shouldBe 2
  }

  behavior of "exercise 6.10"

  it should "return the unit of a new state transition for constant A" in {
    State.unit(2).run(SimpleRNG(0))._1 shouldBe 2
  }

  it should "return the mapped random int when f applied" in {
    State.map(RNG2.nonNegativeNextInt)(_ + 1).run(SimpleRNG(0))._1 shouldBe 1
  }

  it should "return the combination of both random ints when f applied" in {
    val g = RNG2.nonNegativeNextInt

    State.map2(g, g)(_ + _ + 1).run(SimpleRNG(0))._1 shouldBe 1
  }

  it should "return a state with the empty list when the empty list given" in {
    State.sequence(Nil).run(SimpleRNG(0))._1 shouldBe Nil
  }

  it should "return a state with the list of generated numbers when non-empty-list given" in {
    val g  = RNG2.nonNegativeNextInt
    val fs = g :: g :: Nil

    State.sequence(fs).run(SimpleRNG(0))._1 shouldBe 0 :: 0 :: Nil
  }

  behavior of "exercise 6.11"

  it should "return the initial state of the machine when the empty list of inputs given" in {
    State.toA(Machine(false, 10, 5), simulateMachine(Nil)) shouldBe Tuple2(10, 5)
  }

  it should "accept a new coin when machine is locked" in {
    State.toA(Machine(true, 10, 5), simulateMachine(List(Coin))) shouldBe Tuple2(11, 5)
  }

  it should "reject a new coin when machine is unlocked" in {
    State.toA(Machine(false, 10, 5), simulateMachine(List(Coin))) shouldBe Tuple2(10, 5)
  }

  it should "reject a new coin when machine is locked and no candies left" in {
    State.toA(Machine(true, 10, 0), simulateMachine(List(Coin))) shouldBe Tuple2(10, 0)
  }

  it should "return a candy when machine is unlocked" in {
    State.toA(Machine(false, 11, 5), simulateMachine(List(Turn))) shouldBe Tuple2(11, 4)
  }

  it should "return no candy when machine is locked" in {
    State.toA(Machine(true, 11, 5), simulateMachine(List(Turn))) shouldBe Tuple2(11, 5)
  }

  it should "return the last state when a list of inputs given" in {
    val inputs = Coin :: Turn :: Coin :: Turn :: Coin :: Turn :: Coin :: Turn :: Nil

    State.toA(Machine(true, 10, 5), simulateMachine(inputs)) shouldBe Tuple2(14, 1)
  }
}
