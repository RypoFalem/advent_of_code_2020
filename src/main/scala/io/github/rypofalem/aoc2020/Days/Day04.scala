package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Days.Day04.Passport.credTypes
import io.github.rypofalem.aoc2020.Problem

object Day04 extends Problem {
  override def uid: Int = 4
  lazy val passports: Array[Passport] = {
    val allCreds = (lines mkString "\n") split "\n\n"
    allCreds map Passport.fromString
  }

  val requirements: Map[String, Requirement] = {
    val requirementsList = List(BirthYear, IssueYear, ExpirationYear, Height, HairColor, EyeColor, PassportID)
    (requirementsList.map(_.key) zip requirementsList).toMap
  }

  case class Passport(creds: Map[String, String])

  object Passport{
    private val credPattern = """(.+):(.+)""".r
    val credTypes: Map[String, String] =
      Map("byr" -> "Birth Year", "iyr" -> "Issue Year", "eyr" -> "Expiration Year", "hgt" -> "Height",
        "hcl" -> "Hair Color", "ecl" -> "Eye Color", "pid" -> "Passport ID", "cid" -> "Country ID")

    def fromString(str: String): Passport = {
      val passport = for {
        cred <- str.split(Array(' ', '\n'))
        credPattern(key, value) = cred
      } yield key -> value
      Passport(passport.toMap)
    }
  }

  trait Requirement{
    val key: String
    def valid(value: String): Boolean
  }

  trait YearRequirement extends Requirement {
    val min: Int
    val max: Int
    override def valid(value: String): Boolean = {
      if(value.length != 4) false
      else {
        val n = value.toInt
        min <= n && n <= max
      }
    }
  }

  object BirthYear extends YearRequirement {
    override val key: String = "byr"
    override val min: Int = 1920
    override val max: Int = 2002
  }

  object IssueYear extends YearRequirement {
    override val min: Int = 2010
    override val max: Int = 2020
    override val key: String = "iyr"
  }

  object ExpirationYear extends YearRequirement {
    override val min: Int = 2020
    override val max: Int = 2030
    override val key: String = "eyr"
  }

  object Height extends Requirement {
    override val key: String = "hgt"
    private val pattern = """(\d+)(\w+)""".r
    override def valid(value: String): Boolean = value match {
      case pattern(inches, "in") =>
        val in = inches.toInt
        59 <= in && in <= 76
      case pattern(centimeters, "cm") =>
        val cm = centimeters.toInt
        150 <= cm && cm <= 193
      case _ => false
    }
  }

  object HairColor extends Requirement {
    override val key: String = "hcl"
    private val pattern = """#(\d|[a-f]){6}""".r
    override def valid(value: String): Boolean = pattern.matches(value)
  }

  object EyeColor extends Requirement {
    override val key: String = "ecl"
    private val validTokens = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    override def valid(value: String): Boolean = validTokens.contains(value)
  }

  object PassportID extends Requirement {
    override val key: String = "pid"
    private val pattern = """\d+""".r
    override def valid(value: String): Boolean = value.length == 9 && pattern.matches(value)
  }

  def validate1(pp: Passport): Boolean = {
    val fakeTypes = credTypes - "cid"
    fakeTypes.forall{case (k, v) => pp.creds.contains(k)}
  }

  def validate2(pp: Passport): Boolean = {
    requirements.forall{
      case (id, requirement) => pp.creds.contains(id) && requirement.valid(pp.creds(id))
    }
  }


  override def solution1: String = {
    val count = passports count validate1
    s"There are $count 'valid' passports"
  }

  override def solution2: String = {
    val count = passports count validate2
    s"There are $count 'valid' passports"
  }
}
