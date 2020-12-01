package io.github.rypofalem.aoc2020

trait Problem {
  def uid: Int
  def solution1: String
  def solution2: String
  override def toString: String = s"Problem $uid:\n\t$solution1\n\t$solution2"

  def lines: List[String] = Util.loadResourceAsList(f"Day$uid%02d.txt")
}
