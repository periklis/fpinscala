package org.fpinscala.chapters.seven

import org.fpinscala.chapters.seven.Exercises._
import org.scalatest.{FlatSpec, Matchers}

class ExercisesSpec extends FlatSpec with Matchers {

  behavior of "exercise 7.1"

  it should "return f-mapped result of the two parallel computations" in {
    Par.get(Par.map2(Par.unit(2))(Par.unit(3))(_ + _)) shouldBe Par.get(Par.unit(5))
  }
}
