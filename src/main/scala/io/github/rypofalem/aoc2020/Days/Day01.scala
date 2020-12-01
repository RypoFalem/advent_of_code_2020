package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Problem

object Day01 extends Problem {
  private lazy val nums = lines map (_.toInt)
  private lazy val lazyIndices = nums.indices.to(LazyList)

  override def uid: Int = 1

  override def solution1: String = {
    val lazyProduct = for {
      i <- lazyIndices
      j <- lazyIndices
      if i != j
      if nums(i) + nums(j) == 2020
    } yield nums(i) * nums(j)
    s"The product of the two numbers that add to 2020 is ${lazyProduct.head}"
  }

  override def solution2: String = {
    val lazyProduct = for {
      i <- lazyIndices
      j <- lazyIndices
      if nums(i) + nums(j) <= 2020 // speed things up a lot
      k <- lazyIndices
      if Set(i, j, k).size == 3
      if nums(i) + nums(j) + nums(k) == 2020
    } yield nums(i) * nums(j) * nums(k)
    s"The product of the three numbers that add to 2020 is ${lazyProduct.head}"
  }
}
