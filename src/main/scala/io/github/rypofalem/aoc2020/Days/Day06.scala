package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Problem

object Day06 extends Problem{
  override def uid: Int = 6
  lazy val groups: Array[String] = lines.mkString("\n").split("\n\n")

  lazy val countYes: Array[Int] = {
    def uniqueLetters(str: String): Int = str.toSet.count(_.isLetter)
    groups map uniqueLetters
  }

  lazy val countAllYes: Array[Int] = {
    def uniqueLetters(str: String): Int =
      str.split("\n").map(_.toSet).reduce[Set[Char]]((a,b) => a intersect b).count(_.isLetter)
    groups map uniqueLetters
  }

  override def solution1: String = s"The sum of all unique yes's in all groups is ${countYes.sum}"
  override def solution2: String = s"The sum of all unique unanimous yes's in all groups is ${countAllYes.sum}"
}
