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

  final case class RangeCutResult(
      leftRemainder: Option[Range],
      intersect: Option[Range],
      rightRemainder: Option[Range]
  )
  final case class Range(start: Long, length: Long) {
    val end: Long = start + length

    def cut(that: Range): RangeCutResult = {
      val leftRemainder =
        if (that.start < start)
          val newEnd = Math.min(this.start, that.end)
          Some(Range(that.start, newEnd - that.start))
        else None

      val rightRemainder =
        if (that.end > end) {
          val newStart = Math.max(that.start, end)
          Some(Range(newStart, that.end - newStart))
        } else None

      val intersect =
        if (that.start <= this.start && that.end >= this.end)
          // This is fully covered by that
          Some(this)
        else if (that.start >= this.start && that.end <= this.end)
          // That is fully covered by this
          Some(that)
        else if (that.start <= this.start && that.end >= this.start)
          // That reaches into this from the left
          Some(Range(this.start, that.end - this.start))
        else if (
          that.start >= this.start && that.start <= this.end && that.end >= this.end
        )
          // That reaches into this from the right
          Some(Range(that.start, this.end - that.start))
        else
          // No overlap
          Option.empty[Range]

      RangeCutResult(leftRemainder, intersect, rightRemainder)
    }
  }

  final case class MapRangeResult(
      result: Option[Range],
      remainders: List[Range]
  )

  final case class MappingRange(destination: Long, source: Long, length: Long) {
    private val sourceRange = Range(source, length)
    private val delta = destination - source

    def map(n: Long): Option[Long] =
      if (n >= source && n < source + length)
        Some(n - source + destination)
      else
        None

    /** */
    def mapRange(range: Range): MapRangeResult =
      val rangeCut = sourceRange.cut(range)
      val intersectMapping = rangeCut.intersect.map { intersect =>
        Range(intersect.start + delta, intersect.length)
      }
      val remainders =
        rangeCut.leftRemainder.toList ++ rangeCut.rightRemainder.toList

      MapRangeResult(intersectMapping, remainders)
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
