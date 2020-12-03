package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Problem

object Day03 extends Problem{
  override def uid: Int = 3
  lazy val terrainPattern: Vector[String] = lines.toVector
  val tree: Char = '#'
  val patternLength: Int = terrainPattern(0).length

  def isTree(down:Int, right:Int): Boolean = terrainPattern(down).charAt(right % patternLength) == tree

  def calcCollisions(d :Int, r:Int): Int = {
    val treeCollisions = for {
      i <- 1 until terrainPattern.length
      right = i * r
      down = i * d
      if down < terrainPattern.length
      if isTree(down, right)
    } yield (down, right)
    treeCollisions.length
  }

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
