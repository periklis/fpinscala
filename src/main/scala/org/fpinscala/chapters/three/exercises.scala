package org.fpinscala.chapters.three

object Exercises {

  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil     => as
      case _ :: xs => xs
    }

  def setHead[A](a: A, as: List[A]): List[A] =
    as match {
      case ls @ _ if a != Nil => a :: ls
      case _                  => as
    }

  def drop[A](n: Int, as: List[A]): List[A] =
    as match {
      case _ :: xs if n != 0 => drop(n - 1, xs)
      case _                 => as
    }

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case x :: xs if f(x) => dropWhile(xs)(f)
      case _               => as
    }

  def init[A](as: List[A]): List[A] =
    as match {
      case Nil                  => Nil
      case x :: xs if xs != Nil => x :: init(xs)
      case _ :: Nil             => Nil
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil     => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil     => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }

  object LeftFold {
    def sum(as: List[Int]): Int =
      foldLeft(as, 0)(_ + _)

    def product(as: List[Double]) =
      foldLeft(as, 1.0)(_ * _)

    def append[A](as: List[A], bs: List[A]): List[A] =
      foldLeft(bs, as)((b, x) => b :+ x)

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, List[A]())((z, x) => x :: z)

    def flatten[A](as: List[List[A]]): List[A] =
      foldLeft(as, Nil: List[A])((z, a) => z ::: foldLeft(a, Nil: List[A])((b, a) => b :+ a))
  }

  object RightFold {
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      as match {
        case Nil     => z
        case x :: xs => foldLeft(xs, f(z, x))((b, a) => foldRight(List(a), b)((a, b) => f(b, a)))
      }

    def append[A](as: List[A], bs: List[A]): List[A] =
      foldRight(as, bs)((x, b) => x :: b)

    def sum(as: List[Int]): Int =
      foldRight(as, 0)(_ + _)

    def product(as: List[Double]): Double =
      foldRight(as, 1.0)(_ * _)

    def length[A](as: List[A]): Int =
      foldRight(as, 0)((_, z) => z + 1)
  }

  object TailRecursive {
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil     => z
        case x :: xs => foldRight(xs, f(x, z))((a, b) => foldLeft(List(a), b)((a, b) => f(b, a)))
      }
  }

  def incByOne(as: List[Int]): List[Int] =
    as match {
      case Nil     => Nil
      case x :: xs => (x + 1) :: incByOne(xs)
    }

  def stringify(as: List[Double]): List[String] =
    as match {
      case Nil     => Nil
      case x :: xs => (x.toString) :: stringify(xs)
    }

  def map[A, B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil     => Nil
      case x :: xs => f(x) :: map(xs)(f)
    }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil             => Nil
      case x :: xs if f(x) => x :: filter(xs)(f)
      case _ :: xs         => filter(xs)(f)
    }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    as match {
      case Nil     => Nil
      case x :: xs => f(x) ::: flatMap(xs)(f)
    }

  object FlatMap {
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def concatAdd(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (a, Nil)           => a
      case (Nil, b)           => b
      case (x :: xs, y :: ys) => (x + y) :: concatAdd(xs, ys)
    }

  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] =
    (as, bs) match {
      case (a, Nil)           => a
      case (Nil, b)           => b
      case (x :: xs, y :: ys) => List(f(x, y)) ::: zipWith(xs, ys)(f)
    }

  def take[A](as: List[A], n: Int): List[A] =
    as match {
      case Nil              => Nil
      case x :: xs if n > 1 => x :: take(xs, n - 1)
      case x :: _ if n > 0  => List(x)
      case _ if n <= 0      => Nil
    }

  def takeWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case x :: xs if f(x) => x :: takeWhile(xs)(f)
      case _               => Nil
    }

  def forall[A](as: List[A])(f: A => Boolean): Boolean =
    as match {
      case Nil             => true
      case x :: xs if f(x) => forall(xs)(f)
      case _               => false
    }

  def exists[A](as: List[A], a: A): Boolean =
    as match {
      case Nil               => false
      case x :: xs if x != a => exists(xs, a)
      case x :: _ if x == a  => true
    }

  def scanLeft[A, B](as: List[A], z: B)(f: (B, A) => B): List[B] =
    as match {
      case Nil     => Nil
      case x :: xs => f(z, x) :: scanLeft(xs, f(z, x))(f)
    }

  def scanRight[A, B](as: List[A], z: B)(f: (A, B) => B): List[B] = {
    val reversed = LeftFold.reverse(as)

    def innerScanRight(as: List[A], z: B)(f: (A, B) => B): List[B] =
      as match {
        case Nil     => Nil
        case x :: xs => innerScanRight(xs, f(x, z))(f) :+ f(x, z)
      }

    innerScanRight(reversed, z)(f)
  }

  def hasSubsequence[A](as: List[A], ns: List[A]): Boolean =
    as match {
      case Nil => false
      case _ :: xs =>
        if (take(as, ns.length) == ns) true
        else hasSubsequence(xs, ns)
    }

  sealed trait Tree[+A]
  case class Leaf[A](value: A)                     extends Tree[A]
  case class Branch[A](lhs: Tree[A], rhs: Tree[A]) extends Tree[A]

  object Tree {
    def apply[A](value: A): Tree[A] =
      new Leaf(value)

    def apply[A](lhs: Tree[A], rhs: Tree[A]): Tree[A] =
      new Branch(lhs, rhs)

    def size[A](t: Tree[A]): Int =
      t match {
        case Leaf(_)          => 1
        case Branch(lhs, rhs) => size(lhs) + size(rhs) + 1
      }

    def max(t: Tree[Int]): Int =
      t match {
        case Leaf(v)          => v
        case Branch(lhs, rhs) => max(lhs) max max(rhs)
      }

    def depth[A](t: Tree[A]): Int =
      t match {
        case Leaf(_)          => 1
        case Branch(lhs, rhs) => (depth(lhs) max depth(rhs)) + 1
      }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      t match {
        case Leaf(v)          => Leaf(f(v))
        case Branch(lhs, rhs) => Branch(map(lhs)(f), map(rhs)(f))
      }
  }

  object TreeFold {
    def fold[A, B](t: Tree[A], f: A => B)(g: (B, B) => B): B =
      t match {
        case Leaf(v)          => f(v)
        case Branch(lhs, rhs) => g(fold(lhs, f)(g), fold(rhs, f)(g))
      }

    def size(t: Tree[Int]): Int =
      fold[Int, Int](t, _ => 1)(_ + _ + 1)

    def max(t: Tree[Int]): Int =
      fold[Int, Int](t, a => a)(_ max _)

    def depth(t: Tree[Int]): Int =
      fold[Int, Int](t, _ => 1)((a, b) => (a max b) + 1)

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold[A, Tree[B]](t, a => Tree(f(a)))(Tree(_, _))
  }
}
