package org.fpinscala.chapters.eight

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
}
