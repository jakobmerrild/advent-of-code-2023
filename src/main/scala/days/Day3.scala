package days

import days.Day3.Coordinates

import scala.util.matching.Regex

class Day3(input: String) extends Puzzle {
  lazy val symbolCoordinates = Day3.extractSymbolCoordinates(input)
  lazy val numbers = input
    .split("\n")
    .zipWithIndex
    .flatMap { case (s, y) => Day3.extractNumbers(s, y) }

  lazy val gearCoordinates = Day3.extractGearCoordinates(input)
  override def solve: String =
    numbers
      .filter { case (number, coordinates) =>
        val surroundingCoordinates = coordinates.flatMap(_.surrounding)
        surroundingCoordinates.intersect(symbolCoordinates).nonEmpty
      }
      .map { case (number, coordinates) => number }
      .sum
      .toString

  override def solve2: String =
    gearCoordinates.toList
      .map { coordinates =>
        val surroundingNumbers = numbers.collect {
          case (i, cs) if coordinates.surrounding.intersect(cs).nonEmpty => i
        }
        if (surroundingNumbers.length == 2)
          surroundingNumbers(0) * surroundingNumbers(1)
        else 0
      }
      .sum
      .toString
}

object Day3 {
  val symbolRegex = "[^.0-9]".r
  val gearRegex = "\\*".r
  val numberRegex = "\\d+".r
  final case class Coordinates(x: Int, y: Int) {
    override def toString: String = s"($x, $y)"
    def surrounding: Set[Coordinates] = Set(
      Coordinates(x - 1, y - 1),
      Coordinates(x, y - 1),
      Coordinates(x + 1, y - 1),
      Coordinates(x - 1, y),
      Coordinates(x + 1, y),
      Coordinates(x - 1, y + 1),
      Coordinates(x, y + 1),
      Coordinates(x + 1, y + 1)
    )

    def withinBoundS(xMax: Int, yMax: Int): Boolean =
      x >= 0 && x < xMax && y >= 0 && y < yMax
  }

  def extractNumbers(
      s: String,
      yCoordinate: Int
  ): List[(Int, Set[Coordinates])] =
    numberRegex
      .findAllIn(s)
      .matchData
      .map { m =>
        val number = m.group(0).toInt
        val coordinates =
          (m.start until m.end).map(x => Coordinates(x, yCoordinate)).toSet
        (number, coordinates)
      }
      .toList

  def extractSymbolCoordinates(input: String): Set[Coordinates] =
    extract(input, symbolRegex)

  def extractGearCoordinates(input: String): Set[Coordinates] =
    extract(input, gearRegex)

  private def extract(input: String, regex: Regex): Set[Coordinates] =
    val inputChars: Array[Array[Char]] = input.split("\n").map(_.toCharArray)
    inputChars.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex
        .filter { case (c, _) => regex.matches(c.toString) }
        .map { case (_, x) => Coordinates(x, y) }
    }.toSet
}
