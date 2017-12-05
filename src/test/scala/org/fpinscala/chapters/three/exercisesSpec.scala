package org.fpinscala.chapters.three

import org.fpinscala.chapters.three.Exercises._
import org.scalatest.{FlatSpec, Matchers}

class ExercisesSpec extends FlatSpec with Matchers {

  behavior of "exercise 3.2"

  it should "return the tail of a list" in {
    val as = "aaa" :: "bbb" :: "ccc" :: Nil;

    tail(as) shouldBe "bbb" :: "ccc" :: Nil;
  }

  behavior of "exercise 3.3"

  it should "return the list as with a new head x" in {
    val as = "aaa" :: "bbb" :: "ccc" :: Nil;

    setHead("zzz", as) shouldBe "zzz" :: as
  }

  it should "return the list as if new head is 'Nil'" in {
    val as = "aaa" :: "bbb" :: "ccc" :: Nil;

    setHead(Nil, as) shouldBe as
  }

  behavior of "exercise 3.4"

  it should "return the input list after droping 0 elements" in {
    val as = "aaa" :: "bbb" :: "ccc" :: Nil;

    drop(0, as) shouldBe as
  }

  it should "return the sublist after droping n elements" in {
    val as = "aaa" :: "bbb" :: "ccc" :: Nil;

    drop(2, as) shouldBe "ccc" :: Nil;
  }

  it should "return the empty list after droping any elements" in {
    val as = Nil;

    drop(4, as) shouldBe Nil
  }

  behavior of "exercise 3.5"

  it should "return Nil if input list is the empty list" in {
    val as   = Nil
    val pred = (_: Any) => true

    dropWhile(as)(pred) shouldBe Nil
  }

  it should "return the sublist until the predicate is false for an element" in {
    val as      = "aaa" :: "bb" :: "ccc" :: Nil;
    val lengthp = (x: String) => x.length == 3

    dropWhile(as)(lengthp) shouldBe "bb" :: "ccc" :: Nil
  }

  it should "return the empty list when the predidate is true for each element" in {
    val as      = "aaa" :: "bbb" :: "ccc" :: Nil;
    val lengthp = (x: String) => x.length == 3

    dropWhile(as)(lengthp) shouldBe Nil
  }

  behavior of "exercise 3.6"

  it should "return Nil if empty list" in {
    val as = Nil

    init(as) shouldBe Nil
  }

  it should "return a list of all elements but the last one" in {
    val as = "aaa" :: "bbb" :: "ccc" :: Nil

    init(as) shouldBe "aaa" :: "bbb" :: Nil
  }

  behavior of "exercise 3.7 - foldRight"

  it should "return init accumulator value when empty List" in {
    val as: List[Int] = Nil

    foldRight(as, 0)(_ + _) shouldBe 0
  }

  it should "return accumulated value when applying binary operation on list" in {
    val as = 1 :: 2 :: 3 :: Nil

    foldRight(as, 0)(_ + _) shouldBe 6
  }

  behavior of "exercise 3.7 - sum"

  it should "returns 0 when empty list" in {
    sum(Nil) shouldBe 0
  }

  it should "return the total sum of all list elements" in {
    val as = 1 :: 2 :: 3 :: Nil

    sum(as) shouldBe 6
  }

  behavior of "exercise 3.7 - product"

  it should "return 1.0 when empty list" in {
    product(Nil) shouldBe 1.0
  }

  it should "return the total product of all list elements" in {
    val as = 1.0 :: 2.0 :: 3.0 :: Nil

    product(as) shouldBe 6.0
  }

  behavior of "exercise 3.9"

  it should "return 0 for the empty list" in {
    val as = Nil

    Exercises.length(as) shouldBe 0
  }

  it should "return the total length of the list" in {
    val as = 1 :: 2 :: 3 :: Nil

    Exercises.length(as) shouldBe 3
  }

  behavior of "exercise 3.10"

  it should "return init accumulator value when empty list" in {
    val as: List[Int] = Nil

    foldLeft(as, 0)(_ + _) shouldBe 0
  }

  it should "return the accumulated value applying binary operation on list" in {
    val as = 1 :: 2 :: 3 :: Nil

    foldLeft(as, 0)(_ + _) shouldBe 6
  }

  behavior of "exercise 3.11 - sumlf"

  it should "return 0 when empty list" in {
    sumlf(Nil) shouldBe 0
  }

  it should "return the total sum of all list elements" in {
    val as = 1 :: 2 :: 3 :: Nil

    sumlf(as) shouldBe 6
  }

  behavior of "exercise 3.11 - productlf"

  it should "return 1.0 when empty list" in {
    productlf(Nil) shouldBe 1.0
  }

  it should "return the total product of all list elements" in {
    val as = 1.0 :: 2.0 :: 3.0 :: Nil

    productlf(as) shouldBe 6.0
  }

  behavior of "exercise 3.12"

  it should "return 'Nil' on empty list" in {
    reverse(Nil) shouldBe Nil
  }

  it should "return a list with all elements in reverse order" in {
    val as = 1 :: 2 :: 3 :: Nil

    reverse(as) shouldBe 3 :: 2 :: 1 :: Nil
  }

  behavior of "exercise 3.12"

  it should "return init accumulated value when empty List" in {
    val as: List[Int] = Nil

    foldLeftFR(as, 0)(_ + _) shouldBe 0
  }

  it should "return the accumulated value applying the binary operation on all elements" in {
    val as: List[Int] = 1 :: 2 :: 3 :: Nil
    val z: List[Int]  = Nil

    foldLeftFR(as, z)((z, a) => a :: z) shouldBe 3 :: 2 :: 1 :: Nil
  }

  behavior of "exercise 3.13"

  it should "return init accumulated value when empty List" in {
    val as: List[Int] = Nil

    foldRightTR(as, 0)(_ + _) shouldBe 0
  }

  it should "return the accumulated value applying the binary operation on all elements" in {
    val as = 1 :: 2 :: 3 :: Nil

    foldRightTR(as, List[Int]())((a: Int, z: List[Int]) => a :: z) shouldBe 3 :: 2 :: 1 :: Nil
  }

  behavior of "exercise 3.14"

  it should "return a single element list on appending to empty list" in {
    val as: List[Int] = Nil

    append(1, as) shouldBe 1 :: Nil
  }

  it should "return a new list with the new element on last position" in {
    val as = 1 :: 2 :: 3 :: Nil

    append(4, as) shouldBe 1 :: 2 :: 3 :: 4 :: Nil
  }
}
