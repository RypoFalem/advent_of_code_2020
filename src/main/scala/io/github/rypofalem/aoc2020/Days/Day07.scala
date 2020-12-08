package io.github.rypofalem.aoc2020.Days

import io.github.rypofalem.aoc2020.Problem

import scala.annotation.nowarn
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

object Day07 extends Problem{
  override def uid: Int = 7
  lazy val rules: Map[Color, Contents] = (for{
    bagRule <- lines map BagRule.apply
  } yield bagRule.color -> bagRule.contents).toMap

  type Color = String
  type Contents = Map[Color, Int]
  case class BagRule(color: Color, contents: Map[Color, Int]){
    override def toString: Color = s"$color bags can hold: $contents"
  }
  object BagRule extends BagRuleParser {
    def apply(str: String): BagRule = parse(bagRule, str).get
  }

  trait BagRuleParser extends JavaTokenParsers {
    override val whiteSpace: Regex = """[\s\.]+""".r
    override def skipWhitespace: Boolean = true
    // light yellow bags contain 4 striped aqua bags, 1 dotted aqua bag, 2 dark orange bags, 4 faded olive bags.
    def color: Parser[Color]                = """[\w\s]+?(?=bags?)""".r ^^ (_.trim)
    def freq: Parser[Int]                   = wholeNumber ^^ {_.toInt}
    def freqColor: Parser[Map[Color, Int]]  = freq ~ color ^^ { case f ~ c => Map(c -> f) }
    def contents: Parser[Map[Color, Int]]   = repsep(freqColor, """bags?,""".r) ^^
      (listFC => if (listFC.isEmpty) Map() else listFC.reduce { (a, b) => a ++ b })
    def bagRule: Parser[BagRule]            = (color <~ "bags contain") ~ contents ^^ {
      case color ~ contents => BagRule(color, contents)
    }
  }

  def bagsThatCanContain(color: Color): Set[Color] = {
    def sub(color:Color, seen: Set[Color]): Set[Color] = {
      val containers = rules filter { case (_, contents) => contents.contains(color)}
      val newColors = containers.keySet diff seen
      if(newColors.isEmpty) seen
      else newColors flatMap (sub(_, seen ++ newColors))

    }
    sub(color, Set())
  }

  def bagsInsideOf(color: Color): MapCount[Color] = {
    val inside = rules(color)
    inside.foldLeft(new MapCount[Color](Map())) {
      case (mc, (c, n)) =>
        val recursiveBags = bagsInsideOf(c).toMap map { case (color, l) => (color, l*n)}
          mc + (c,n) ++ recursiveBags
    }
  }

  class MapCount[T](internalMap: Map[T,Long]) {
    def toMap: Map[T, Long] = internalMap

    def ++(that: Map[T, Long]): MapCount[T] = this.++(new MapCount(that))

    def ++(that: MapCount[T]): MapCount[T] = {
      that.toMap.foldLeft(this){ case (accum, (elem, n)) => accum + (elem, n)}
    }

    def +(elem: T, n: Long): MapCount[T] = {
      if(internalMap.contains(elem)) new MapCount(internalMap + (elem -> (internalMap(elem) + n)))
      else new MapCount(internalMap + (elem -> n))
    }

    lazy val sum: Long = internalMap.foldLeft(0L){case (sum, (_,n)) => sum + n}
  }

  override def solution1: String = {
    val n = bagsThatCanContain("shiny gold").size.toString
    s"There are $n bags that can contain shiny gold bags"
  }

  override def solution2: String = {
    val sum = bagsInsideOf("shiny gold").sum
    s"$sum bags fit inside of one shiny gold bag"
  }
}