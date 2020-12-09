package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Problem

import scala.annotation.tailrec
import scala.collection.mutable


object Day09 extends Problem{
  override def uid: Int = 9
  val nums: Vector[Long] = (lines map (_.toLong)).toVector
  val preamble: Int = 25
  val targetSum: Long = 18272118

  def isValid(index: Int): Boolean = {
    (for {
      x <- (index - preamble) until index
      y <- (index - preamble) until index
      if x != y
      if nums(x) + nums(y) == nums(index)
    } yield (x, y)).nonEmpty

  }

  override def solution1: String = {
    val n = nums.zipWithIndex.splitAt(25)._2.
      find({ case (n, index) => !isValid(index) }).get._1
    s"the first invalid number is $n"
  }

  override def solution2: String = {
    @tailrec
    def loop(nums: List[Long], accum: mutable.Queue[Long]): Seq[Long] ={
      if(accum.sum == targetSum) accum.toIndexedSeq
      else if(nums.isEmpty) throw new IllegalStateException("Solution not found")
      else {
        accum.enqueue(nums.head)
        accum.dequeueWhile(_ => accum.sum > targetSum)
        loop(nums.tail, accum)
      }
    }
    val xs = loop(nums.toList, mutable.Queue())
    val weakness = xs.min + xs.max
    s"The weakness is $weakness"
  }
}
