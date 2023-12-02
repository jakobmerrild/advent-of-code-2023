package days

import munit.FunSuite

class Day1Test extends FunSuite {
  val firstInput = """1abc2
                    |pqr3stu8vwx
                    |a1b2c3d4e5f
                    |treb7uchet""".stripMargin

  val secondInput = """two1nine
                      |eightwothree
                      |abcone2threexyz
                      |xtwone3four
                      |4nineeightseven2
                      |zoneight234
                      |7pqrstsixteen
                      |rzvlkjvone142oneightpv""".stripMargin

  test("Day1.Part1") {
    val actual = new Day1(firstInput).solve
    assertEquals(actual, "142")
  }
  test("Day1.Part2") {
    val actual = new Day1(secondInput).solve2
    assertEquals(actual, "299")
  }
}
