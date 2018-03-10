package org.fpinscala.chapters.eight

import org.fpinscala.chapters.six.Exercises.SimpleRNG
import org.fpinscala.chapters.eight.Exercises._
import org.scalatest.{FlatSpec, Matchers}

case class SimplePropC(val r: Boolean) extends SimpleProp { var res = r }

class ExercisesSpec extends FlatSpec with Matchers {

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

  it should "return generate either false or true" in {
    Gen.boolean.sample.run(SimpleRNG(123))._1 shouldBe false
  }
}
