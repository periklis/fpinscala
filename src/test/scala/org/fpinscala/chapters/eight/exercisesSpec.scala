package org.fpinscala.chapters.eight

import org.fpinscala.chapters.six.Exercises.SimpleRNG
import org.fpinscala.chapters.eight.Exercises._
import org.scalatest.{FlatSpec, Inspectors, Matchers}

case class SimplePropC(val r: Boolean) extends SimpleProp { var res = r }

class ExercisesSpec extends FlatSpec with Matchers with Inspectors {

  behavior of "exercise 8.3"

  it should "return true if both properties are valid" in {
    val rhs = SimplePropC(true)
    val lhs = SimplePropC(true)

    (lhs && rhs).check shouldBe true
  }

  it should "return false if either property is invalid" in {
    val rhs = SimplePropC(true)
    val lhs = SimplePropC(false)

    (lhs && rhs).check shouldBe false
  }

  behavior of "exercise 8.4"

  it should "return a generated Integer number between start and exluding stop" in {
    val g: Gen[Int] = Gen.choose(1, 300)

    g.sample.run(SimpleRNG(123))._1 should (be >= 1 and be < 300)
  }

  behavior of "exercise 8.5"

  it should "return the constant value inside a Gen container" in {
    Gen.unit(2).sample.run(SimpleRNG(123))._1 shouldBe 2
  }

  it should "return generate false" in {
    Gen.boolean.sample.run(SimpleRNG(123))._1 shouldBe false
  }

  it should "return generate true" in {
    Gen.boolean.sample.run(SimpleRNG(99))._1 shouldBe true
  }

  it should "return a list of generator instances" in {
    val gs = Gen.listOfN(10, Gen.choose(1, 300))

    forAll(gs.sample.run(SimpleRNG(123))._1) { _ should (be >= 1 and be < 300) }
  }

  behavior of "exercise 8.6"

  it should "return the combination of two different generators" in {
    val g1 = Gen.choose(1, 300)
    val f  = (n: Int) => Gen.choose(n, n * 2)

    Gen.flatMap(g1)(f).sample.run(SimpleRNG(123))._1 should (be >= 14 and be < 28)
  }

  it should "return a list of generator based on a generated size" in {
    val g1 = Gen.choose(1, 100)
    val g2 = Gen.choose(1, 300)

    forAll(Gen.listOfN(g1, g2).sample.run(SimpleRNG(123))._1) { _ should (be >= 1 and be < 100) }
  }

  behavior of "exercise 8.7"

  it should "return a new generator that produces the union of generated values" in {
    val g1 = Gen.choose(1, 100)
    val g2 = Gen.choose(1, 3)

    Gen.union(g1, g2).sample.run(SimpleRNG(123))._1 should (be >= 1 and be < 3)
    Gen.union(g1, g2).sample.run(SimpleRNG(99))._1 should (be >= 1 and be < 100)
  }

  behavior of "exercise 8.8"

  it should "return the generator according to its weight" in {
    val g1 = Gen.choose(1, 100)
    val g2 = Gen.choose(1, 3)

    Gen.weighted((g1, 0.3), (g2, 0.7)).sample.run(SimpleRNG(123))._1 should (be >= 1 and be < 3)
    Gen.weighted((g1, 0.3), (g2, 0.7)).sample.run(SimpleRNG(99))._1 should (be >= 1 and be < 100)
  }

  behavior of "exercise 8.9"

  it should "check that all generated values pass the predicate" in {
    Prop.forAll(Gen.choose(1, 3))(a => a >= 1 && a < 3).run(100, SimpleRNG(123)) shouldEqual (Passed)
  }

  it should "return the failed case and the sucess count when the predicate is not passed" in {
    Prop.forAll(Gen.choose(1, 3))(a => a >= 2 && a < 3).run(100, SimpleRNG(123)) shouldEqual (Failed(
      "Failed: 1 Passed: 2",
      2))
  }

  it should "return Passed if both properties pass" in {
    import Prop.Implicits._
    val lhs = Prop.forAll(Gen.choose(1, 3))(a => a >= 1 && a < 3)
    val rhs = Prop.forAll(Gen.choose(1, 100))(a => a >= 1 && a < 100)

    (lhs && rhs).run(100, SimpleRNG(123)) shouldEqual (Passed)
  }

  it should "return Failed if one of the properties fails" in {
    import Prop.Implicits._
    val lhs = Prop.forAll(Gen.choose(1, 3))(a => a >= 2 && a < 3)
    val rhs = Prop.forAll(Gen.choose(1, 100))(a => a >= 1 && a < 100)

    (lhs && rhs).run(100, SimpleRNG(123)) shouldEqual (Failed("Failed: 1 Passed: 2", 2))
  }

  it should "return Failed if both properties fail" in {
    import Prop.Implicits._
    val lhs = Prop.forAll(Gen.choose(1, 3))(a => a >= 2 && a < 3)
    val rhs = Prop.forAll(Gen.choose(1, 100))(a => a >= 1 && a < 2)

    (lhs && rhs).run(100, SimpleRNG(123)) shouldEqual (Failed("Left: Failed: 1 Passed: 2 Right: Failed: 14 Passed: 1",
                                                              3))
  }

  it should "return Passed if one of the properties pass" in {
    import Prop.Implicits._
    val lhs = Prop.forAll(Gen.choose(1, 3))(a => a >= 2 && a < 3)
    val rhs = Prop.forAll(Gen.choose(1, 100))(a => a >= 1 && a < 100)

    (lhs || rhs).run(100, SimpleRNG(123)) shouldEqual (Passed)
  }

  it should "return Failed if both of the properties fail" in {
    import Prop.Implicits._
    val lhs = Prop.forAll(Gen.choose(1, 3))(a => a >= 2 && a < 3)
    val rhs = Prop.forAll(Gen.choose(1, 100))(a => a >= 1 && a < 2)

    (lhs || rhs).run(100, SimpleRNG(123)) shouldEqual (Failed("Left: Failed: 1 Passed: 2 Right: Failed: 14 Passed: 1",
                                                              3))

  }
}
