package days

class Day5(input: String) extends Puzzle {

  val seedRangeRegex = """(\d+) (\d+)""".r

  val seeds = input.split("\n")(0).split(" ").drop(1).map(_.toLong).toList
  val mappings = input.split("\n\n").drop(1).map(Day5.Mapping(_)).toList
  val seedRanges =
    seedRangeRegex
      .findAllMatchIn(input.split("\n")(0))
      .map { m =>
        Day5.Range(m.group(1).toLong, m.group(2).toLong)
      }
      .toList

  private def findLocation(seed: Long): Long =
    mappings.foldLeft(seed) { (acc, mapping) =>
      mapping.map(acc)
    }

  override def solve: String =
    seeds.map(findLocation).min.toString

  private def mappedRanges(range: Day5.Range): List[Day5.Range] =
    mappings.foldLeft(List(range)) { (acc, mapping) =>
      acc.flatMap(mapping.mapRange)
    }

  override def solve2: String =
    seedRanges.flatMap(mappedRanges).minBy(_.start).start.toString
}

object Day5:
  final case class Range(start: Long, length: Long)

  final case class MapRangeResult(
      result: Option[Range],
      remainders: List[Range]
  )

  final case class MappingRange(destination: Long, source: Long, length: Long) {
    def map(n: Long): Option[Long] =
      if (n >= source && n < source + length)
        Some(n - source + destination)
      else
        None

    /** */
    def mapRange(range: Range): MapRangeResult =
      // range is empty
      if (range.length == 0)
        MapRangeResult(None, Nil)
      // range is not covered by this mapping
      else if (
        range.start >= source + length || range.start + range.length <= source
      )
        MapRangeResult(None, List(range))
      // range is fully covered by this mapping
      else if (
        range.start >= source && range.start + range.length <= source + length
      )
        MapRangeResult(
          Some(Range(range.start - source + destination, range.length)),
          Nil
        )
      // range is stretches over this map
      else if (
        range.start <= source && range.start + range.length >= source + length
      )
        MapRangeResult(
          Some(Range(destination, length)),
          List(
            Range(range.start, source - range.start),
            Range(source + length, range.start + range.length - source - length)
          )
        )

      // range is partially covered by this map from the left
      else if (range.start <= source && range.start + range.length > source) {
        MapRangeResult(
          Some(Range(destination, range.length - source + range.start)),
          List(Range(range.start, range.start + range.length - source))
        )
      } // range is partially covered by this map from the right
      else {
        val remainderLength = source + length - range.start
        MapRangeResult(
          Some(Range(range.start, source + length - range.start)),
          List(
            Range(source + length, range.start + range.length - source - length)
          )
        )
      }
  }

  object MappingRange:
    def apply(s: String): MappingRange = {
      val parts = s.split(" ")
      val destination = parts(0).toLong
      val source = parts(1).toLong
      val length = parts(2).toLong
      MappingRange(destination, source, length)
    }
  end MappingRange

  final case class Mapping(ranges: List[MappingRange]) {
    def map(n: Long): Long =
      ranges
        .collectFirst {
          case range: MappingRange if range.map(n).isDefined => range.map(n).get
        }
        .getOrElse(n)

    def mapRange(range: Range): List[Range] =
      val (results, remainders) = ranges
        .foldLeft((List.empty[Range], List(range))) {
          case ((mappedRanges, remainders), mapping) =>
            remainders match {
              case Nil =>
                (mappedRanges, Nil)
              case remainderRanges =>
                val mapResults = remainderRanges.map(mapping.mapRange)
                (
                  mappedRanges ++ mapResults.flatMap(_.result),
                  mapResults.flatMap(_.remainders)
                )
            }
        }
      remainders ++ results
  }

  object Mapping:
    def apply(s: String): Mapping = {
      val ranges = s.split("\n").drop(1).map(MappingRange(_)).toList
      Mapping(ranges)
    }
  end Mapping

end Day5
