package org.fpinscala.chapters.eight

import org.fpinscala.chapters.eight.Exercises.max
import org.scalacheck._
import org.scalacheck.Prop._

object MaxSpecification extends Properties("Max List") {

  property("empty list has none defined maximum") = forAll(Gen.const(List())) { as: List[Int] =>
    throws(classOf[java.lang.UnsupportedOperationException])(max(as))
  }

  property("every pos list item is lte maximum value") = forAll(Gen.nonEmptyListOf(Gen.posNum[Int])) { as: List[Int] =>
    as.forall(_ <= max(as))
  }

  property("every neg list item is lte maximum value") = forAll(Gen.nonEmptyListOf(Gen.negNum[Int])) { as: List[Int] =>
    as.forall(_ <= max(as))
  }

  property("all list items are equal to max if same value list") = forAll(Gen.nonEmptyListOf(Gen.const(2))) {
    as: List[Int] =>
      as.forall(_ == max(as))
  }
}
