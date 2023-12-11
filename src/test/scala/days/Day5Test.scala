package days

import munit.{FunSuite, ScalaCheckSuite}
import Day5.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

class Day5Test extends FunSuite with ScalaCheckSuite {
  test("MappingRange.map") {
    val range = MappingRange(52, 50, 48)
    val inRange = range.map(50)
    val outOfRange = range.map(98)
    assertEquals(inRange, Some(52L))
    assertEquals(outOfRange, None)
  }

  test("MappingRange.mapRange fully covered") {
    val range = MappingRange(52, 50, 48)
    val fullRange = range.mapRange(Range(50, 48))
    assertEquals(fullRange, MapRangeResult(Some(Range(52, 48)), Nil))
    val partialRange = range.mapRange(Range(56, 18))
    assertEquals(partialRange, MapRangeResult(Some(Range(58, 18)), Nil))
  }

  test("MappingRange.mapRange not covered") {
    val range = MappingRange(52, 50, 48)
    val belowRange = range.mapRange(Range(1, 49))
    assertEquals(belowRange, MapRangeResult(None, List(Range(1, 49))))
    val aboveRange = range.mapRange(Range(98, 12))
    assertEquals(aboveRange, MapRangeResult(None, List(Range(98, 12))))
  }

  test("MappingRange.mapRange partially covered from left") {
    val range = MappingRange(52, 50, 48)
    val onStart = range.mapRange(Range(50, 1))
    assertEquals(onStart, MapRangeResult(Some(Range(52, 1)), Nil))
    val overlaps = range.mapRange(Range(45, 10))
    assertEquals(
      overlaps,
      MapRangeResult(Some(Range(52, 5)), List(Range(45, 5)))
    )
  }

  test("MappingRange.mapRange partially covered from right") {
    val range = MappingRange(52, 50, 48)
    val overlaps = range.mapRange(Range(95, 10))
    assertEquals(
      overlaps,
      MapRangeResult(Some(Range(95, 3)), List(Range(98, 7)))
    )
  }

  test("MappingRange.mapRange over covered") {
    val range = MappingRange(52, 50, 48)
    val overCovered = range.mapRange(Range(1, 100))
    assertEquals(
      overCovered,
      MapRangeResult(Some(Range(52, 48)), List(Range(1, 49), Range(98, 3)))
    )
  }

  test("checking stuff") {
    val x = Range(20, 10)
    val ys = List(
      Range(0, 10), // completely on the left
      Range(10, 15), // overlaps from the left
      Range(22, 5), // completely inside
      Range(0, 40), // overlaps on both sides
      Range(25, 12), // overlaps from the right
      Range(40, 20), // completely on the right
      Range(20, 10) // identity
    )
    ys.foreach { y =>
      val result = x.cut(y)
      val resultLength = result.leftRemainder.map(_.length).getOrElse(0L) +
        result.intersect.map(_.length).getOrElse(0L) +
        result.rightRemainder.map(_.length).getOrElse(0L)
      assertEquals(resultLength, y.length)
    }
  }

  val rangeGenerator: Gen[Range] = for {
    start <- Gen.choose(0, Int.MaxValue)
    length <- Gen.choose(1, Int.MaxValue)
  } yield Range(start.toLong, length.toLong)

  implicit val arbRange: Arbitrary[Range] = Arbitrary(rangeGenerator)

  property("Range.cut") {
    forAll { (x: Range, y: Range) =>
      val result = x.cut(y)
      val resultLength = result.leftRemainder.map(_.length).getOrElse(0L) +
        result.intersect.map(_.length).getOrElse(0L) +
        result.rightRemainder.map(_.length).getOrElse(0L)

      assertEquals(resultLength, y.length)
      assertEquals(y.start < x.start, result.leftRemainder.isDefined)
      assertEquals(y.end > x.end, result.rightRemainder.isDefined)
    }
  }

  property("MappingRange.mapRange result should never change total length") {
    forAll {
      (
          mappingSource: Int,
          mappingDest: Int,
          mappingLength: Int,
          rangeStart: Int,
          rangeLength: Int
      ) =>
        val range = MappingRange(
          Math.abs(mappingDest.toLong),
          Math.abs(mappingSource.toLong),
          Math.abs(mappingLength.toLong)
        )
        val startLong = Math.abs(rangeStart.toLong)
        val lengthLong = Math.abs(rangeLength.toLong)
        val result = range.mapRange(Range(startLong, lengthLong))
        val totalLength =
          result.result.map(_.length).getOrElse(0L) + result.remainders
            .map(_.length)
            .sum
        totalLength == lengthLong
    }
  }

  test("Mapping.map") {
    val ranges = List(
      MappingRange(52, 50, 48),
      MappingRange(50, 98, 2)
    )
    val mapping = Mapping(ranges)
    assertEquals(mapping.map(50), 52L)
    assertEquals(mapping.map(98), 50L)
    assertEquals(mapping.map(2), 2L)
  }

  test("Mapping.mapRange") {
    val ranges = List(
      MappingRange(52, 50, 48),
      MappingRange(50, 98, 2)
    )
    val mapping = Mapping(ranges)
    val range = Range(0, 100)
    val result = mapping.mapRange(range)
    val expected = Set(Range(52, 48), Range(50, 2), Range(0, 50))
    assertEquals(result.toSet, expected)
  }

  test("Day5.solve2") {
    val input = """seeds: 79 14 55 13
                  |
                  |seed-to-soil map:
                  |50 98 2
                  |52 50 48
                  |
                  |soil-to-fertilizer map:
                  |0 15 37
                  |37 52 2
                  |39 0 15
                  |
                  |fertilizer-to-water map:
                  |49 53 8
                  |0 11 42
                  |42 0 7
                  |57 7 4
                  |
                  |water-to-light map:
                  |88 18 7
                  |18 25 70
                  |
                  |light-to-temperature map:
                  |45 77 23
                  |81 45 19
                  |68 64 13
                  |
                  |temperature-to-humidity map:
                  |0 69 1
                  |1 0 69
                  |
                  |humidity-to-location map:
                  |60 56 37
                  |56 93 4""".stripMargin

    val day5 = new Day5(input)
    val result = day5.solve2
    assertEquals(result, "46")
  }
}
