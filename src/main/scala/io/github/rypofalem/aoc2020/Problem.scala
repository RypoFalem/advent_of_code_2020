package io.github.rypofalem.aoc2020

import scala.util.{Failure, Success, Try}

trait Problem {
  def uid: Int
  def solution1: String
  def solution2: String

  override def toString: String = {
    def trySolution(solution: => String): String = Try(solution) match {
      case Success(value) => value
      case Failure(exception) =>
        exception.printStackTrace()
        exception.toString
    }
    val s1 = trySolution(solution1)
    val s2 = trySolution(solution2)
    s"Problem $uid:\n\t$s1\n\t$s2"
  }

  def lines: List[String] = Util.loadResourceAsList(f"Day$uid%02d.txt")

  def main(args: Array[String]): Unit = println(toString)
}
