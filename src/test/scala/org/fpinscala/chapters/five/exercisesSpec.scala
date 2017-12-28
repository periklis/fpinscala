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

  behavior of "exercise 5.4"

  it should "return false when empty stream given" in {
    exists(Empty)(_ == 1) shouldBe false
  }

  it should "return false when element not part of stream" in {
    val as = Stream(2, 3, 4, 5)
    exists(as)(_ == 7) shouldBe false
  }

  it should "return true when element part of stream" in {
    val as = Stream(2, 3, 4, 5)
    exists(as)(_ == 3) shouldBe true
  }

  it should "return initial accumulator value when folding right the empty stream" in {
    foldRight(Empty)(0)((_, _) => 2) shouldBe 0
  }

  it should "retutn the acculumated value when folding right the empty stream" in {
    val as = Stream(2, 3, 4, 5)

    foldRight(as)(0)((a, b) => a + b) shouldBe 14
  }

  it should "return false when applying predicate for all elements of the empty stream" in {
    forAll(Empty)(_ == 1) shouldBe false
  }

  it should "return false when predicate fails for at least one element of the stream" in {
    val as = Stream(2, 3, 4, 6)
    val f  = (a: Int) => BigInt(a).mod(2) == 0

    forAll(as)(f) shouldBe false
  }

  it should "return true when predicate passes for all elements of the stream" in {
    val as = Stream(2, 4, 6, 8)
    val f  = (a: Int) => BigInt(a).mod(2) == 0

    forAll(as)(f) shouldBe true
  }

  behavior of "exercise 5.5"

  it should "return the empty stream when predicate fails on first element using foldRight" in {
    val as = Stream(2, 3, 4, 5, 6)
    WithFoldRight.takeWhile(as)(_ => false) shouldBe Empty
  }

  it should "return the empty stream when taking by predicate from empty stream using foldRight" in {
    WithFoldRight.takeWhile(Empty)(_ => true) shouldBe Empty
  }

  it should "return a new stream with first elements that pass the predicate when non-empty stream given using foldRight" in {
    val as         = Stream(2, 4, 6, 3, 5, 7)
    val evenNumber = (a: Int) => BigInt(a).mod(2) == 0

    toList(WithFoldRight.takeWhile(as)(evenNumber)) should contain allElementsOf (toList(Stream(2, 4, 6)))
  }

  behavior of "exercise 5.6"

  it should "return None when empty stream given" in {
    headOption(Empty) shouldBe None
  }

  it should "return Some head value when non empty stream given" in {
    val as = Stream(2, 3, 4, 5)
    headOption(as) shouldBe Some(2)
  }

  it should "return None when empty stream given using foldRight" in {
    WithFoldRight.headOption(Empty) shouldBe None
  }

  it should "return Some head value when non empty stream given using foldRight" in {
    val as = Stream(2, 3, 4, 5)
    WithFoldRight.headOption(as) shouldBe Some(2)
  }

  behavior of "exercise 5.7 - map"

  it should "return the empty stream when mapping over the empty stream" in {
    map(Empty)(a => a) shouldBe Empty
  }

  it should "return the lazy mapped stream when non empty stream given" in {
    val as = Stream(2, 3, 4, 5)

    toList(map(as)(_ + 1)) should contain allElementsOf (toList(Stream(3, 4, 5, 6)))
  }

  behavior of "exercise 5.7 - filter"

  it should "return the empty stream when filtering over the empty stream" in {
    filter(Empty)(_ => true) shouldBe Empty
  }

  it should "return the stream of elements passing the filter predicate when non empty stream given" in {
    val as         = Stream(2, 3, 4, 5, 6, 7, 8)
    val evenNumber = (a: Int) => BigInt(a).mod(2) == 0

    toList(filter(as)(evenNumber)) should contain allElementsOf (toList(Stream(2, 4, 6, 8)))
  }

  behavior of "exercise 5.7 - append"

  it should "return a single element stream when appending one element to the empty stream" in {
    toList(append(Empty)(Stream(1))) should contain allElementsOf (toList(Stream(1)))
  }

  it should "return a new stream with all elements plus the one given when appending one element to a non empty stream" in {
    val as = Stream(1, 2, 3, 4, 5)
    toList(append(as)(Stream(6))) should contain allElementsOf (toList(Stream(1, 2, 3, 4, 5, 6)))
  }

  behavior of "exercise 5.7 - flatMap"

  it should "return the empty stream when flat-mapping over the empty stream" in {
    flatMap(Empty)(a => Stream(a)) shouldBe Empty
  }

  it should "return the lazy flat-mapped stream when non empty stream given" in {
    val as = Stream(2, 3, 4, 5)

    toList(flatMap(as)(a => Stream(a + 1))) should contain allElementsOf (toList(Stream(3, 4, 5, 6)))
  }

  behavior of "exercise 5.11"

  it should "return the empty stream when state function terminates on start" in {
    unfold(1)(_ => None) shouldBe Empty
  }

  it should "return a stream on n elements when state functions runs n times" in {
    val as = unfold(1)(s => if (s != 3) Some((s, s + 1)) else None)

    toList(as) should contain allElementsOf (toList(Stream(1, 2)))
  }
}
