package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Problem

object Day03 extends Problem {
  override def uid: Int = 3
  lazy val terrain: Vector[String] = lines.toVector
  val tree: Char = '#'
  lazy val terrainWidth: Int = terrain(0).length

  def isTree(down:Int, right:Int): Boolean = terrain(down).charAt(right % terrainWidth) == tree

  def calcCollisions(downSpeed: Int, rightSpeed: Int): Int =
    (1 until terrain.length / downSpeed) count (move => isTree(move * downSpeed, move * rightSpeed))

  override def solution1: String = {
    val collisions = calcCollisions(1,3)
    s"$collisions collisions detected"
  }

  override def solution2: String = {
    val product = (List((1,1), (1,3), (1,5), (1,7), (2,1)) map {case (d,r) => BigInt(calcCollisions(d,r))}).product
    s"product of collisions on all paths is $product"
  }

  def main(args:Array[String]): Unit = println(toString)
}
