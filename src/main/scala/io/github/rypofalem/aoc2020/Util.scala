package io.github.rypofalem.aoc2020

import java.io.{File, InputStream, PrintWriter}

import scala.io.{BufferedSource, Source}

object Util {

  def loadResource(fileName: String): BufferedSource = {
    val stream: InputStream = getClass.getResourceAsStream(s"/$fileName")
    Source.fromInputStream( stream )
  }

  def loadResourceAsList(fileName: String): List[String] = loadResource(fileName).getLines.toList

  def writeToFile(str: String, filename: String): Unit = {
    val file = new File(System.getProperty("user.dir") + "\\$filename")
    file.createNewFile()
    val pw = new PrintWriter(file)
    pw.write(str)
    pw.close()
  }
}
