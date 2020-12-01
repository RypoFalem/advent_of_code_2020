package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Problem

object Day01 extends Problem {
  override def uid: Int = 1

  private val nums = lines map (_.toInt)
  private val lazyIndices = nums.indices.to(LazyList)

  def generalSolution(nums: List[Int], entries: Int, targetSum: Int): List[List[Int]] = {
    val sortedNums = nums.sorted
    def partialSolution(entries: Int, accr: List[List[Int]], numsPicked: List[Int], numsLeft: List[Int]): List[List[Int]] = {
      // base case: if it's successful, add it to accr otherwise just return accr
      if(entries < 1) if(numsPicked.sum == targetSum) numsPicked :: accr else accr

      // base case: we ran out of numbers, return accr
      else if(numsLeft.isEmpty) accr

      // find sub-solutions and add them to accr
      else {
        val sum = numsPicked.sum
        val mostRecentNum = numsPicked.headOption
        val validCandidates = numsLeft.to(LazyList).zipWithIndex
          // don't waste time evaluating elements that sum too high
          .takeWhile {case (n,i) => n + sum <= targetSum}
          // eliminate duplicates: only look at numbers higher than the most recent picked number
          .filter {case (n,i) => (mostRecentNum map (n >= _)).getOrElse(true)}
        def remove[T](l : List[T], i: Int): List[T] = l.take(i) ::: l.takeRight(l.length - 1 - i)
        val solutions = for{
          (c,i) <- validCandidates
          solutions <- partialSolution(entries - 1, List(), c :: numsPicked, remove(numsLeft, i))
        } yield solutions
        solutions.toList ::: accr
      }
    }
    partialSolution(entries, List(), List(), sortedNums)
  }

  def s1: List[List[Int]] = generalSolution(nums, 2, 2020) // List(List(1412, 608))
  def s2: List[List[Int]] = generalSolution(nums, 3, 2020) // List(List(833, 782, 405))

  override def solution1: String = s"The product of the two numbers that add to 2020 is ${s1.head.product}"
  override def solution2: String = s"The product of the three numbers that add to 2020 is ${s2.head.product}"
}
