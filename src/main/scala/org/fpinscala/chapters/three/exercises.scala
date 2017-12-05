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

  def sum(as: List[Int]): Int =
    foldRight(as, 0)(_ + _)

  def product(as: List[Double]): Double =
    foldRight(as, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, z) => z + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil     => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }

  def sumlf(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def productlf(as: List[Double]) =
    foldLeft(as, 1.0)(_ * _)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((z, x) => x :: z)

  def foldLeftFR[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil     => z
      case x :: xs => foldLeft(xs, f(z, x))((b, a) => foldRight(List(a), b)((a, b) => f(b, a)))
    }

  def foldRightTR[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil     => z
      case x :: xs => foldRightTR(xs, f(x, z))((a, b) => foldLeft(List(a), b)((a, b) => f(b, a)))
    }

  def append1[A](a: A, as: List[A]): List[A] =
    as match {
      case Nil     => a :: Nil
      case x :: xs => x :: append(a, xs)
    }

  def append[A](a: A, as: List[A]): List[A] =
    foldLeft(as, List[A](a))((b, x) => x :: b)
}
