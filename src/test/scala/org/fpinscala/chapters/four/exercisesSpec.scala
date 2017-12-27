package org.fpinscala.chapters.four

import org.fpinscala.chapters.four.Exercises._
import org.fpinscala.chapters.four.Exercises.Option._
import org.fpinscala.chapters.four.Generators._
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class ExercisesSpec extends FlatSpec with Matchers with PropertyChecks {

  behavior of "exercise 4.1 - Option"

  it should "return map to None if Option None" in {
    val ob: Option[Int] = None
    map(ob)(_ + 1) shouldBe None
  }

  it should "return mapped option for Some value" in {
    val ob = Some(1)
    map(ob)(_ + 1) shouldBe Some(2)
  }

  it should "return None when flatMap on Option None" in {
    val ob: Option[Int] = None

    flatMap(ob)(a => Some(a + 1)) shouldBe None
  }

  it should "return option of mapped value for Some value" in {
    val ob = Some(1)
    flatMap(ob)(a => Some(a.toString)) shouldBe Some("1")
  }

  it should "return default value when option None" in {
    val ob: Option[Int] = None

    getOrElse(ob)(2) shouldBe 2
  }

  it should "return initial value when option of some value" in {
    val ob = Some(1)

    getOrElse(ob)(2) shouldBe 1
  }

  it should "return other option when option None" in {
    val ob: Option[Int] = None
    val oe              = Some(1)

    orElse(ob)(oe) shouldBe oe
  }

  it should "return intial option when some option value" in {
    val ob = Some(1)
    val oe = Some(2)

    orElse(ob)(oe) shouldBe ob
  }

  it should "return None when option None" in {
    val ob: Option[Int] = None

    filter(ob)(a => true) shouldBe None
  }

  it should "return the option value when filter predicate applies" in {
    val ob = Some(1)

    filter(ob)(BigInt(_).mod(2) != 0) shouldBe ob
  }

  it should "return None when filter predicate does not apply" in {
    val ob = Some(1)

    filter(ob)(BigInt(_).mod(2) == 0) shouldBe None
  }

  behavior of "exericse 4.2"

  it should "return None when empty sequence" in {
    val xs: Seq[Double] = Seq()

    variance(xs) shouldBe None
  }

  it should "return the calculated variance for a sequence of doubles" in {
    val xs = Seq(1.0, 2.0, 3.0, 4.0)

    variance(xs) shouldBe Some(1.25)
  }

  behavior of "exercise 4.3"

  it should "return None when a single option argument is None" in {
    val a: Option[Int] = None
    val b              = Some(2)
    val f              = (a: Int, b: Int) => a + b

    map2(a, b)(f) shouldBe None
  }

  it should "return calculated value when applying f on bowth option arguments" in {
    val a = Some(1)
    val b = Some(2)
    val f = (a: Int, b: Int) => (a + b).toString

    map2(a, b)(f) shouldBe Some("3")
  }

  behavior of "exercise 4.4"

  it should "return None when empty List" in {
    val as = Nil

    sequence(as) shouldBe None
  }

  it should "return None if any element is None" in {
    val as = Some(2) :: None :: Some(3) :: Nil

    sequence(as) shouldBe None
  }

  it should "return an option of a list of values when some option value" in {
    val as = Some(2) :: Some(4) :: Some(3) :: Nil

    sequence(as) shouldBe Some(2 :: 4 :: 3 :: Nil)
  }

  behavior of "exercise 4.5"

  it should "return None when f applied on an elements returns None" in {
    val as = Some(2) :: None :: Some(3) :: Nil

    traverse(as)(a => Some(a)) shouldBe None
  }

  it should "return list of transformed elements" in {
    val as = Some(2) :: Some(4) :: Some(3) :: Nil

    traverse(as)(a => Some(a + 1)) shouldBe Some(List(3, 5, 4))
  }

  it should "return None when traversing empty List" in {
    traverseSinglePass(Nil: List[Option[Int]])(Some(_)) shouldBe None
  }

  it should "return None when f applied on an elements returns None on single pass" in {
    val as = Some(2) :: None :: Some(3) :: Nil

    traverseSinglePass(as)(Some(_)) shouldBe None
  }

  it should "return list of transformed elements on single pass" in {
    val as = Some(2) :: Some(4) :: Some(3) :: Nil

    traverseSinglePass(as)(a => Some(a + 1)) shouldBe Some(List(3, 5, 4))
  }

  it should "return list of transformed elements on single pass for gen lists" in {
    val f = (a: Int) => a + 1

    forAll(traversableOf(Gen.posNum[Int])(f))({
      case (actual, expected) => {
        traverseSinglePass(actual)(a => Some(f(a))) shouldBe expected
      }
    })
  }

  behavior of "exercise 4.6"

  it should "return the left value when failure case given to map" in {
    val e = Left("An error occured")

    Either.map(e)(a => a) shouldBe e
  }

  it should "return the right mapped value when success case given to map" in {
    val r = Right(1)

    Either.map(r)(_ + 1) shouldBe Right(2)
  }

  it should "return the left value when failure case given to flatten and map" in {
    val e = Left("An error occured")

    Either.flatMap(e)(a => a) shouldBe e
  }

  it should "return the right mapped value when success case given to flatten and map" in {
    val r = Right(1)

    Either.flatMap(r)(a => Right(a + 1)) shouldBe Right(2)
  }

  it should "return left value when sucess case given but map fails" in {
    val e = Left("An error occured in map")
    val r = Right(1)

    Either.flatMap(r)(_ => e) shouldBe e
  }

  it should "return the original value when success case given" in {
    val e = Right(1)
    val o = Right(2)

    Either.orElse(e)(o) shouldBe e
  }

  it should "return the other right value when failure case given" in {
    val e = Left("Error case")
    val o = Right(1)

    Either.orElse(e)(o) shouldBe o
  }

  it should "return the other failure case when both fail" in {
    val e = Left("Error case")
    val o = Left("Other error case")

    Either.orElse(e)(o) shouldBe o
  }

  it should "return the left lhs value if any failure case given" in {
    val e = Left("Error case")
    val o = Right(1)

    Either.map2(e)(o)((a, _) => a) shouldBe e
  }

  it should "retutn the left rhs value if any failure case given" in {
    val e = Right(1)
    val o = Left("Error case")

    Either.map2(e)(o)((a, _) => a) shouldBe o
  }

  it should "return the right mapped value if both success cases given" in {
    val e1 = Right(1)
    val e2 = Right(2)

    Either.map2(e1)(e2)(_ + _) shouldBe Right(3)
  }

  behavior of "exercise 4.7"

  it should "return the right empty list value when empty list given" in {
    Either.sequence(Nil) shouldBe Right(Nil)
  }

  it should "return the left value when at least one failure case in list given" in {
    val es = Right(2) :: Right(3) :: Left("error") :: Right(4) :: Nil

    Either.sequence(es) shouldBe Left("error")
  }

  it should "retutn the right list of values when only success cases in list given" in {
    val es = Right(2) :: Right(3) :: Right(5) :: Right(4) :: Nil

    Either.sequence(es) shouldBe Right(2 :: 3 :: 5 :: 4 :: Nil)
  }

  it should "return the right Nil value when empty list given" in {
    Either.traverse(Nil)(Right(_)) shouldBe Right(Nil)
  }

  it should "retutn the right mapped values list when map returns only sucess case" in {
    val as = 1 :: 2 :: 3 :: Nil

    Either.traverse(as)(a => Right(a + 1)) shouldBe Right(2 :: 3 :: 4 :: Nil)
  }

  it should "retutn the left mapped value when map fails at least once" in {
    val as = 1 :: 2 :: 3 :: Nil
    val f  = (a: Int) => if (a % 2 !== 0) Right(1) else Left("Not even number")

    Either.traverse(as)(f) shouldBe Left("Not even number")
  }
}
