package org.fpinscala.chapters.two

object Exercises {

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def solve(n: Int, a: Int, b: Int): Int =
      n match {
        case 0 => a
        case _ => solve(n - 1, b, a + b)
      }

    solve(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (as.isDefinedAt(n) && !ordered(as(n - 1), as(n))) false
      else loop(n + 1)

    loop(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def andThen[A, B, C](f: A => B, g: B => C): A => C =
    (a: A) => g(f(a)) // or g compose f
}
