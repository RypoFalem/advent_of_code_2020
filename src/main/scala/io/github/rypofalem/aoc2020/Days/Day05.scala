package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Problem

object Day05 extends Problem{
  override def uid: Int = 5
  lazy val seats: List[Int] = lines map codeToID

  def codeToID(pass: String): Int =
    (pass map { case 'F' | 'L' => 0; case 'B' | 'R' => 1}) // replace letters with binary digits
      .reduceLeft[Int]{case (id, digit) => (id << 1) + digit} // reduce the sequence of binary digits to a single int

  override def solution1: String = s"Highest seat ID: ${seats.max}"

  override def solution2: String = {
    val sorted = seats.sorted.to(LazyList)
    val first = sorted.head
    // example case where first = 30
    // 30 31 32 34 <- pass
    // 0  1  2  3  <- index
    // 30 30 30 31  <- {pass - index} 34 is the first to skip (34 - index != first), meaning 33 is my seat
    val seat = sorted.zipWithIndex.find{ case (pass, index) => pass - index != first }.get._1 - 1
    s"My seat is $seat"
  }

  def main(args:Array[String]): Unit = println(toString)
}
