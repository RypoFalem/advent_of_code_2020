package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Problem

object Day02 extends Problem {
  override def uid: Int = 2
  lazy val pws: List[PasswordSetting] = lines map PasswordSetting.fromString

  object PasswordSetting{
    private val settingsPattern = """(\d+)-(\d+) (\w): (.*)""".r
    def fromString(line: String): PasswordSetting = line match {
      case settingsPattern(min, max, char, pw) => PasswordSetting(min.toInt, max.toInt, char.charAt(0), pw)
    }
  }

  case class PasswordSetting(min: Int, max: Int, char: Char, pw: String) {
    lazy val valid1: Boolean = {
      val count = pw.count(_ == char)
      count >= min && count <= max
    }
    lazy val valid2: Boolean = (pw(min - 1) == char) ^ (pw(max - 1) == char)
  }

  override def solution1: String = s"valid passwords type1: ${pws.count(_.valid1)}"
  override def solution2: String = s"valid passwords type2: ${pws.count(_.valid2)}"
  def main(args: Array[String]): Unit = println(toString)
}
