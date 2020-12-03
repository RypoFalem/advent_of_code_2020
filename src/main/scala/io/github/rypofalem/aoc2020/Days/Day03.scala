package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Problem

object Day03 extends Problem{
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
    val product = BigInt(calcCollisions(1,1)) * calcCollisions(1,3) *
      calcCollisions(1,5) * calcCollisions(1,7) * calcCollisions(2,1)
    s"product of collisions on all paths is $product"
  }

  def main(args:Array[String]): Unit = println(toString)
}
