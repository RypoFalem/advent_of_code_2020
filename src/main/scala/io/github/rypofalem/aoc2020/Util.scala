package io.github.rypofalem.aoc2020

import java.io.InputStream

import scala.io.{BufferedSource, Source}

object Util {

  def loadResource(fileName: String): BufferedSource = {
    val stream: InputStream = getClass.getResourceAsStream(s"/$fileName")
    Source.fromInputStream( stream )
  }

  def loadResourceAsList(fileName: String): List[String] = loadResource(fileName).getLines.toList
}
