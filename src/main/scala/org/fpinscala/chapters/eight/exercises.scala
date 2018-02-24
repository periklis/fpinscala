package org.fpinscala.chapters.eight

object Exercises {

  def sum(as: List[Int]): Int =
    as.foldLeft(0)(_ + _)

  def max(as: List[Int]): Int =
    as.max
}
