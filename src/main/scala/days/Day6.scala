package days

import cats.Reducible
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import cats.syntax.all.*

class Day6(input: String) extends Puzzle {

  val times: Array[Int] =
    input.split("\n")(0).split(" ").drop(1).filter(_.nonEmpty).map(_.toInt)

  val distances: Array[Long] =
    input.split("\n")(1).split(" ").drop(1).filter(_.nonEmpty).map(_.toLong)

  val races: Array[Day6.Race] =
    times.zip(distances).map { case (time, distance) =>
      Day6.Race(time, distance)
    }

  override def solve: String =
    races.foldLeft(1L)(_ * _.marginOfError(identity)).toString

  override def solve2: String = {
    val singleRace = NonEmptyList.fromListUnsafe(races.toList).reduce
    singleRace.marginOfError(_.toLong).toString
  }
}

object Day6 {
  final case class Race(time: Int, distance: Long) {
    def distances(speedIncrease: Int => Long): List[Long] =
      (1 to time).map { atTime =>
        (time - atTime) * (speedIncrease(atTime))
      }.toList

    def marginOfError(speedIncrease: Int => Long): Long =
      distances(speedIncrease).count(_ > distance)
  }

  object Race {
    implicit val semigroup: Semigroup[Race] =
      (a: Race, b: Race) =>
        Race(s"${a.time}${b.time}".toInt, s"${a.distance}${b.distance}".toLong)
  }
}
