package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Problem

object Day01 extends Problem {
  override def uid: Int = 1
  lazy val nums: List[Int] = lines map (_.toInt)

  def generalSolution(nums: List[Int], entries: Int, targetSum: Int): List[List[Int]] = {
    def partialSolution(entries: Int, accr: List[List[Int]], numsPicked: List[Int], numsLeft: Vector[Int]): List[List[Int]] = {
      // base case: if it's successful, add it to accr otherwise just return accr
      if(entries < 1) if(numsPicked.sum == targetSum) numsPicked :: accr else accr

      // base case: we ran out of numbers, return accr
      else if(numsLeft.isEmpty) accr

      // find sub-solutions and add them to accr
      else {
        val sum = numsPicked.sum
        val mostRecentNum = numsPicked.headOption
        val validCandidates = numsLeft.zipWithIndex
          // don't waste time evaluating elements that sum too high
          .takeWhile {case (n,i) => n + sum <= targetSum}
          // eliminate duplicates: only look at numbers higher than the most recent picked number
          .filter {case (n,i) => mostRecentNum forall (n >= _)}
        def remove[T](v : Vector[T], i: Int): Vector[T] = v.take(i) ++ v.takeRight(v.length - i - 1)
        val solutions = for{
          (c,i) <- validCandidates
          solutions <- partialSolution(entries - 1, List(), c :: numsPicked, remove(numsLeft, i))
        } yield solutions
        solutions ++: accr
      }
    }
    partialSolution(entries, List(), List(), nums.toVector.sorted)
  }

  def s1: List[List[Int]] = generalSolution(nums, 2, 2020) // List(List(1412, 608))
  def s2: List[List[Int]] = generalSolution(nums, 3, 2020) // List(List(833, 782, 405))

  override def solution1: String = s"The product of the two numbers that add to 2020 is ${s1.head.product}"
  override def solution2: String = s"The product of the three numbers that add to 2020 is ${s2.head.product}"
}
