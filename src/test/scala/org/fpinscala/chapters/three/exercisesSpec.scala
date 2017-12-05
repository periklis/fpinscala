package org.fpinscala.chapters.three

import org.fpinscala.chapters.three.Exercises._
import org.fpinscala.chapters.three.Exercises.Tree._
import org.scalatest.{FlatSpec, Matchers, Inspectors}

class ExercisesSpec extends FlatSpec with Matchers with Inspectors {

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
    import org.fpinscala.chapters.three.Exercises.RightFold._

    sum(Nil) shouldBe 0
  }

  it should "return the total sum of all list elements" in {
    import org.fpinscala.chapters.three.Exercises.RightFold._

    val as = 1 :: 2 :: 3 :: Nil

    sum(as) shouldBe 6
  }

  behavior of "exercise 3.7 - product"

  it should "return 1.0 when empty list" in {
    import org.fpinscala.chapters.three.Exercises.RightFold._

    product(Nil) shouldBe 1.0
  }

  it should "return the total product of all list elements" in {
    import org.fpinscala.chapters.three.Exercises.RightFold._

    val as = 1.0 :: 2.0 :: 3.0 :: Nil

    product(as) shouldBe 6.0
  }

  behavior of "exercise 3.9"

  it should "return 0 for the empty list" in {
    val as = Nil

    RightFold.length(as) shouldBe 0
  }

  it should "return the total length of the list" in {
    val as = 1 :: 2 :: 3 :: Nil

    RightFold.length(as) shouldBe 3
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
    import org.fpinscala.chapters.three.Exercises.LeftFold._

    sum(Nil) shouldBe 0
  }

  it should "return the total sum of all list elements" in {
    import org.fpinscala.chapters.three.Exercises.LeftFold._

    val as = 1 :: 2 :: 3 :: Nil

    sum(as) shouldBe 6
  }

  behavior of "exercise 3.11 - productlf"

  it should "return 1.0 when empty list" in {
    import org.fpinscala.chapters.three.Exercises.LeftFold._

    product(Nil) shouldBe 1.0
  }

  it should "return the total product of all list elements" in {
    import org.fpinscala.chapters.three.Exercises.LeftFold._

    val as = 1.0 :: 2.0 :: 3.0 :: Nil

    product(as) shouldBe 6.0
  }

  behavior of "exercise 3.12"

  it should "return 'Nil' on empty list" in {
    import org.fpinscala.chapters.three.Exercises.LeftFold._

    reverse(Nil) shouldBe Nil
  }

  it should "return a list with all elements in reverse order" in {
    import org.fpinscala.chapters.three.Exercises.LeftFold._

    val as = 1 :: 2 :: 3 :: Nil

    reverse(as) shouldBe 3 :: 2 :: 1 :: Nil
  }

  behavior of "exercise 3.12"

  it should "return init accumulated value when empty List" in {
    import org.fpinscala.chapters.three.Exercises.RightFold._

    val as: List[Int] = Nil

    foldLeft(as, 0)(_ + _) shouldBe 0
  }

  it should "return the accumulated value applying the binary operation on all elements" in {
    import org.fpinscala.chapters.three.Exercises.RightFold._

    val as: List[Int] = 1 :: 2 :: 3 :: Nil
    val z: List[Int]  = Nil

    foldLeft(as, z)((z, a) => a :: z) shouldBe 3 :: 2 :: 1 :: Nil
  }

  behavior of "exercise 3.13"

  it should "return init accumulated value when empty List" in {
    import org.fpinscala.chapters.three.Exercises.TailRecursive._

    val as: List[Int] = Nil

    foldRight(as, 0)(_ + _) shouldBe 0
  }

  it should "return the accumulated value applying the binary operation on all elements" in {
    import org.fpinscala.chapters.three.Exercises.TailRecursive._

    val as = 1 :: 2 :: 3 :: Nil

    foldRight(as, List[Int]())((a: Int, z: List[Int]) => a :: z) shouldBe 3 :: 2 :: 1 :: Nil
  }

  behavior of "exercise 3.14"

  it should "return a single element list on appending to empty list by foldLeft" in {
    import org.fpinscala.chapters.three.Exercises.LeftFold._

    val as: List[Int] = Nil

    append(List(1), as) shouldBe 1 :: Nil
  }

  it should "return a new list with the new element on last position by foldLeft" in {
    import org.fpinscala.chapters.three.Exercises.LeftFold._

    val as = 1 :: 2 :: 3 :: Nil
    val bs = 4 :: 5 :: 6 :: Nil

    append(as, bs) shouldBe as ::: bs
  }

  it should "return a single element list on appending to empty list by foldRight" in {
    import org.fpinscala.chapters.three.Exercises.RightFold._

    val as: List[Int] = Nil

    append(List(1), as) shouldBe 1 :: Nil
  }

  it should "return a new list with the new element on last position by foldRight" in {
    import org.fpinscala.chapters.three.Exercises.RightFold._

    val as = 1 :: 2 :: 3 :: Nil
    val bs = 4 :: 5 :: 6 :: Nil

    append(as, bs) shouldBe as ::: bs
  }

  behavior of "exercise 3.15"

  it should "return Nil on empty list" in {
    import org.fpinscala.chapters.three.Exercises.LeftFold._

    val as: List[List[Int]] = Nil

    flatten(as) shouldBe Nil
  }

  it should "return a simple list with all elements from any sublist" in {
    import org.fpinscala.chapters.three.Exercises.LeftFold._

    val as: List[List[Int]] = List(List(1, 2, 3), List(4, 5, 6))
    val expected            = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil

    flatten(as) shouldBe expected
  }

  behavior of "exercise 3.16"

  it should "return Nil when empty list" in {
    incByOne(Nil: List[Int]) shouldBe Nil
  }

  it should "return a new list with incremented elements" in {
    val as = 1 :: 2 :: 3 :: 4 :: Nil

    incByOne(as) shouldBe 2 :: 3 :: 4 :: 5 :: Nil
  }

  behavior of "exercise 3.17"

  it should "return Nil when empty list" in {
    stringify(Nil: List[Double]) shouldBe Nil
  }

  it should "return a list with stringified doubles" in {
    val as = 1.0 :: 2.0 :: 3.0 :: Nil

    stringify(as) shouldBe "1.0" :: "2.0" :: "3.0" :: Nil
  }

  behavior of "exercise 3.18"

  it should "return Nil when empty list" in {
    Exercises.map(Nil: List[Int])(a => a) shouldBe Nil
  }

  it should "return a list with mapped element by given op" in {
    val as = 1 :: 2 :: 3 :: Nil

    Exercises.map(as)(_ + 1) shouldBe 2 :: 3 :: 4 :: Nil
  }

  behavior of "exercise 3.19"

  it should "return Nil when empty list" in {
    filter(Nil: List[Int])(BigInt(_).mod(2) == 0) shouldBe Nil
  }

  it should "return list with even numbers" in {
    val as = 1 :: 2 :: 3 :: 4 :: Nil

    filter(as)(BigInt(_).mod(2) == 0) shouldBe 2 :: 4 :: Nil
  }

  behavior of "exercise 3.20"

  it should "return Nil when empty list" in {
    flatMap(Nil: List[Int])(a => List(a)) shouldBe Nil
  }

  it should "should return a list with doubled elements" in {
    val as = 1 :: 2 :: 3 :: Nil

    flatMap(as)(a => List(a, a)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  behavior of "exercise 3.21"

  it should "return Nil when empty list" in {
    import org.fpinscala.chapters.three.Exercises.FlatMap._

    filter(Nil: List[Int])(BigInt(_).mod(2) == 0) shouldBe Nil
  }

  it should "return list with even numbers" in {
    import org.fpinscala.chapters.three.Exercises.FlatMap._

    val as = 1 :: 2 :: 3 :: 4 :: Nil

    filter(as)(BigInt(_).mod(2) == 0) shouldBe 2 :: 4 :: Nil
  }

  behavior of "exercise 3.22"

  it should "return the non empty input list when the other is Nil" in {
    val as            = 1 :: 2 :: 3 :: Nil
    val bs: List[Int] = Nil

    concatAdd(as, bs) shouldBe as
    concatAdd(bs, as) shouldBe as
  }

  it should "return a new list with th sum of each element pair by position" in {
    val as = 1 :: 2 :: 3 :: Nil
    val bs = 4 :: 5 :: 6 :: Nil

    concatAdd(as, bs) shouldBe 5 :: 7 :: 9 :: Nil
  }

  behavior of "exercise 3.23"

  it should "return then non empty input list when the other is Nil" in {
    val as            = 1 :: 2 :: 3 :: Nil
    val bs: List[Int] = Nil
    val f             = (_: Int, _: Int) => 1

    zipWith(as, bs)(f) shouldBe as
    zipWith(bs, as)(f) shouldBe as
  }

  it should "return the zipped list by applying the binary operation on each element pair" in {
    val as = 1 :: 2 :: 3 :: Nil
    val bs = 4 :: 5 :: 6 :: Nil

    zipWith(as, bs)(_ + _) shouldBe 5 :: 7 :: 9 :: Nil
  }

  behavior of "take n"

  it should "return Nil on empty list" in {
    take(Nil: List[Int], 3) shouldBe Nil
  }

  it should "return a new list with first n elements of init list" in {
    val as = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil

    take(as, 2) shouldBe 1 :: 2 :: Nil
  }

  it should "return Nil if taking zero elements of init list" in {
    val as = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil

    take(as, 0) shouldBe Nil
  }

  it should "return Nil if taking negative amount of elements of init list" in {
    val as = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil

    take(as, -1) shouldBe Nil
  }

  behavior of "takeWhile"

  it should "return Nil on empty list" in {
    val f = (_: Int) => false

    takeWhile(Nil: List[Int])(f) shouldBe Nil
  }

  it should "return a new list of elements until odd number check is true" in {
    val as = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
    val f  = (a: Int) => BigInt(a).mod(2) != 0

    takeWhile(as)(f) shouldBe 1 :: Nil
  }

  behavior of "forall"

  it should "return true if empty list" in {
    val f = (_: Int) => false

    forall(Nil: List[Int])(f) shouldBe true
  }

  it should "return true if all elements are even numbers" in {
    val as = 2 :: 4 :: 6 :: 8 :: 10 :: Nil
    val f  = (a: Int) => BigInt(a).mod(2) == 0

    forall(as)(f) shouldBe true
  }

  it should "return false if on element is not an even number" in {
    val as = 1 :: 2 :: 4 :: 6 :: 8 :: 10 :: Nil
    val f  = (a: Int) => BigInt(a).mod(2) == 0

    forall(as)(f) shouldBe false
  }

  behavior of "exists"

  it should "return false when empty list" in {
    exists(Nil: List[Int], 2) shouldBe false
  }

  it should "return false when element does not exists in list" in {
    val as = 1 :: 2 :: 4 :: 6 :: 8 :: 10 :: Nil

    exists(as, 3) shouldBe false
  }

  it should "return true when element exists in list" in {
    val as = 1 :: 2 :: 4 :: 6 :: 8 :: 10 :: Nil

    exists(as, 6) shouldBe true
  }

  behavior of "scanLeft"

  it should "return Nil when empty list" in {
    scanLeft(Nil: List[Int], 1)((_, _) => 1) shouldBe Nil
  }

  it should "return the list of partial results by applying the binary operation" in {
    val as = 1 :: 2 :: 3 :: Nil

    scanLeft(as, 0)(_ + _) shouldBe 1 :: 3 :: 6 :: Nil
  }

  behavior of "scanRight"

  it should "return Nil when empty list" in {
    scanRight(Nil: List[Int], 1)((_, _) => 1) shouldBe Nil
  }

  it should "return the list of partial results by applying the binary operation" in {
    val as = 1 :: 2 :: 3 :: Nil

    scanRight(as, 0)(_ + _) shouldBe 6 :: 5 :: 3 :: Nil
  }

  behavior of "exercise 3.24"

  it should "return true when needle is the empty list" in {
    val as = 1 :: 2 :: 3 :: Nil

    hasSubsequence(as, Nil: List[Int]) shouldBe true
  }

  it should "return true if needle is a sublist of list" in {
    val as = 1 :: 2 :: 3 :: Nil
    val ns = 2 :: Nil

    hasSubsequence(as, ns) shouldBe true
  }

  it should "return false if needle is not a sublist of list" in {
    val as = 1 :: 2 :: 3 :: Nil
    val ns = 4 :: Nil

    hasSubsequence(as, ns) shouldBe false
  }

  it should "return true if needles is a sublist of list" in {
    val as = 1 :: 2 :: 3 :: 4 :: Nil

    val ns1 = 1 :: 2 :: Nil
    val ns2 = 2 :: 3 :: Nil
    val ns3 = 3 :: 4 :: Nil

    forAll(Seq(ns1, ns2, ns3))((ns) => hasSubsequence(as, ns) shouldBe true)
  }

  behavior of "exercise 3.25"

  it should "return 1 when root" in {
    val tree = Tree(1)

    Tree.size(tree) shouldBe 1
  }

  it should "return the amount of nodes of the tree" in {
    val tree = Tree(Tree(1), Tree(Tree(2), Tree(3)))

    Tree.size(tree) shouldBe 5
  }

  behavior of "exercise 3.26"

  it should "return the root element when single element tree" in {
    val tree = Tree(1)

    max(tree) shouldBe 1
  }

  it should "return the maxium of all nodes of the tree" in {
    val tree = Tree(Tree(1), Tree(Tree(5), Tree(3)))

    max(tree) shouldBe 5
  }

  behavior of "exercise 3.27"

  it should "return 1 when single element tree" in {
    val tree = Tree(1)

    depth(tree) shouldBe 1
  }

  it should "return the maximum depth of a tree" in {
    val tree: Tree[Int] = Tree(
      Tree(
        Tree(1),
        Tree(2)
      ),
      Tree(
        Tree(
          Tree(4),
          Tree(5)
        ),
        Tree(3)
      )
    )

    depth(tree) shouldBe 4
  }

  behavior of "exercise 3.28"

  it should "return the single mapped tree" in {
    val tree: Tree[Int] = Tree(1)
    val f               = (value: Int) => value + 1

    Tree.map(tree)(f) shouldBe Tree(2)
  }

  it should "return the mapped tree by applying f on each leaf" in {
    val tree: Tree[Int] = Tree(Tree(Tree(1), Tree(2)), Tree(Tree(3), Tree(4)))

    val expected: Tree[Int] = Tree(Tree(Tree(2), Tree(3)), Tree(Tree(4), Tree(5)))

    val f = (value: Int) => value + 1

    Tree.map(tree)(f) shouldBe expected
  }

  behavior of "exercise 3.29"

  it should "return 1 when root" in {
    val tree = Tree(1)

    TreeFold.size(tree) shouldBe 1
  }

  it should "return the amount of nodes of the tree" in {
    val tree = Tree(Tree(1), Tree(Tree(2), Tree(3)))

    TreeFold.size(tree) shouldBe 5
  }

  it should "return the root element when single element tree" in {
    val tree = Tree(1)

    TreeFold.max(tree) shouldBe 1
  }

  it should "return the maxium of all nodes of the tree" in {
    val tree = Tree(Tree(1), Tree(Tree(5), Tree(3)))

    TreeFold.max(tree) shouldBe 5
  }

  it should "return 1 when single element tree" in {
    val tree = Tree(1)

    TreeFold.depth(tree) shouldBe 1
  }

  it should "return the maximum depth of a tree" in {
    val tree: Tree[Int] = Tree(
      Tree(
        Tree(1),
        Tree(2)
      ),
      Tree(
        Tree(
          Tree(4),
          Tree(5)
        ),
        Tree(3)
      )
    )

    TreeFold.depth(tree) shouldBe 4
  }

  it should "return the single mapped tree" in {
    val tree: Tree[Int] = Tree(1)
    val f               = (value: Int) => value + 1

    TreeFold.map(tree)(f) shouldBe Tree(2)
  }

  it should "return the mapped tree by applying f on each leaf" in {
    val tree: Tree[Int] = Tree(Tree(Tree(1), Tree(2)), Tree(Tree(3), Tree(4)))

    val expected: Tree[Int] = Tree(Tree(Tree(2), Tree(3)), Tree(Tree(4), Tree(5)))

    val f = (value: Int) => value + 1

    TreeFold.map(tree)(f) shouldBe expected
  }
}
