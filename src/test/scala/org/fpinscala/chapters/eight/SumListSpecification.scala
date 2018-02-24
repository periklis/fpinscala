package org.fpinscala.chapters.eight

import org.fpinscala.chapters.eight.Exercises._
import org.scalacheck._
import org.scalacheck.Prop._

object SumListSpecification extends Properties("Sum List") {

  property("sum order invariant") = forAll { as: List[Int] =>
    sum(as) == sum(as.reverse)
  }

  val sameValueList: Gen[List[Int]] = Gen.nonEmptyListOf(Gen.const(2))

  property("sum of same values equals value times length") = forAll(sameValueList) { as: List[Int] =>
    sum(as) == (as.head * as.length)
  }
}
