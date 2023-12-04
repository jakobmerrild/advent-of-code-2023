package days

import days.Day3.Coordinates
import munit.FunSuite

class Day3Test extends FunSuite {
  test(
    "extractNumbers should return empty list for string with just dots"
  ) {
    assertEquals(Day3.extractNumbers("........", 0), List())
  }

  test("extractNumbers should return list of numbers with coordinates") {
    val actual = Day3.extractNumbers("123...&..456", 0)
    val expected = List(
      (123, Set(Coordinates(0, 0), Coordinates(1, 0), Coordinates(2, 0))),
      (456, Set(Coordinates(9, 0), Coordinates(10, 0), Coordinates(11, 0)))
    )
    assertEquals(actual, expected)
  }

  test("extractSumbolCoordinates should return empty set when no symbols") {
    val input =
      """....1234..12
        |....151..123
        |..12.45.1212""".stripMargin

    val actual = Day3.extractSymbolCoordinates(input)
    assertEquals(actual, Set())
  }

  test("extractSymbolCoordinates should return expected coordinates") {
    val input =
      """...&1234..12
        |.../151=.123
        |..12.%5.1212
        |...€.45.1212""".stripMargin

    val actual = Day3.extractSymbolCoordinates(input)
    val expected = Set(
      Coordinates(3, 0),
      Coordinates(3, 1),
      Coordinates(7, 1),
      Coordinates(5, 2),
      Coordinates(3, 3)
    )
    assertEquals(actual, expected)
  }

  test("solve2 works with example input") {
    val input = """467..114..
                  |...*......
                  |..35..633.
                  |......#...
                  |617*......
                  |.....+.58.
                  |..592.....
                  |......755.
                  |...$.*....
                  |.664.598..""".stripMargin
    val actual = new Day3(input).solve2
    assertEquals(actual, "467835")
  }

  test("solve2 works with number used for multiple gears") {
    val input =
      """12*12
        |...*.
        |..12.""".stripMargin
    val actual = new Day3(input).solve2
    assertEquals(actual, (12 * 12 + 12 * 12).toString)
  }
}
