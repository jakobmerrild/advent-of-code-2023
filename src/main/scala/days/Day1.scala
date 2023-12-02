package days

import scala.util.matching.Regex

final class Day1(input: String) extends Puzzle {

  private val lines = input.split("\n")

  private val digitRegex = "(\\d)".r

  private def parseDigit(number: String): String = {
    number match {
      case digitRegex(digit) => digit
      case "zero"       => "0"
      case "one"        => "1"
      case "two"        => "2"
      case "three"      => "3"
      case "four"       => "4"
      case "five"       => "5"
      case "six"        => "6"
      case "seven"      => "7"
      case "eight"      => "8"
      case "nine"       => "9"
    }
  }
  private def findNumber(line: String, regex: Regex): Int = {
    val matches = regex.findAllIn(line).matchData.map(_.group(1)).toList
    s"${parseDigit(matches.head)}${parseDigit(matches.last)}".toInt
  }

  override def solve: String =
    lines.map(findNumber(_, digitRegex)).sum.toString

  private val numberRegex =
    "(?=(\\d|zero|one|two|three|four|five|six|seven|eight|nine))".r

  override def solve2: String =
    lines.map(findNumber(_, numberRegex)).sum.toString
}
