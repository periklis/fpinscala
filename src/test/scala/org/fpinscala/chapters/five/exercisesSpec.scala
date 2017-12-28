package org.fpinscala.chapters.five

import org.fpinscala.chapters.five.Exercises._
import org.fpinscala.chapters.five.Exercises.Stream._
import org.scalatest.{FlatSpec, Matchers}

class ExercisesSpec extends FlatSpec with Matchers {

  behavior of "exercise 5.1"

  it should "return an empty list when empty stream given" in {
    toList(Empty) shouldBe Nil
  }

  it should "retutn a list of all stream evaluated items when non-empty stream given" in {
    val as = Stream(2, 3, 4, 5, 6)
    toList(as) shouldBe List(2, 3, 4, 5, 6)
  }

  behavior of "exercise 5.2"

  it should "return empty stream when taking zero elements from stream" in {
    val as = Stream(2, 3, 4, 5, 6)
    take(as)(0) shouldBe Empty
  }

  it should "return the empty stream when taking any number of elements from empty stream" in {
    take(Empty)(2) shouldBe Empty
  }

  it should "return a new stream with exact n first elements when non-empty stream given" in {
    val as = Stream(2, 3, 4, 5, 6)
    toList(take(as)(2)) should contain allElementsOf (toList(Stream(2, 3)))
  }

  it should "return the initial stream when dropping zero elements from stream" in {
    val as = Stream(2, 3, 4, 5, 6)
    toList(drop(as)(0)) should contain allElementsOf (toList(as))
  }

  it should "return the empty stream when dropping any number of elements from empty stream" in {
    drop(Empty)(2) shouldBe Empty
  }

  it should "return a new stream with exact size -n tail elements when non-empty stream given" in {
    val as = Stream(2, 3, 4, 5, 6)
    toList(drop(as)(2)) should contain allElementsOf (toList(Stream(4, 5, 6)))
  }

  behavior of "exercise 5.3"

  it should "return the empty stream when predicate fails on first element" in {
    val as = Stream(2, 3, 4, 5, 6)
    takeWhile(as)(_ => false) shouldBe Empty
  }

  it should "return the empty stream when taking by predicate from empty stream" in {
    takeWhile(Empty)(_ => true) shouldBe Empty
  }

  it should "return a new stream with first elements that pass the predicate when non-empty stream given" in {
    val as         = Stream(2, 4, 6, 3, 5, 7)
    val evenNumber = (a: Int) => BigInt(a).mod(2) == 0

    toList(takeWhile(as)(evenNumber)) should contain allElementsOf (toList(Stream(2, 4, 6)))
  }

  it should "return the initial stream when predicate fails to drop by predicate on first element" in {
    val as = Stream(2, 3, 4, 5, 6)
    dropWhile(as)(_ => false) shouldBe as
  }

  it should "return the empty stream when dropping by predicate from empty stream" in {
    dropWhile(Empty)(_ => true) shouldBe Empty
  }

  it should "return a new stream after dropping n elements that pass the predicate when non-empty stream given" in {
    val as         = Stream(2, 4, 6, 3, 5, 7)
    val evenNumber = (a: Int) => BigInt(a).mod(2) == 0

    toList(dropWhile(as)(evenNumber)) should contain allElementsOf (toList(Stream(3, 5, 7)))
  }
}
