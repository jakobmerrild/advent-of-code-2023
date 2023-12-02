package days

class Day2(input: String) extends Puzzle {

  private val lines = input.split("\n").toList
  override def solve: String =
    val limits = Day2.Draw(redCubes = 12, greenCubes = 13, blueCubes = 14)
    lines.view.map(Day2.Game(_)).filter(_.isPossible(limits)).map(_.index).sum.toString

  override def solve2: String =
    lines.view.map(Day2.Game(_)).map(_.mininumDraw.power).sum.toString
}

object Day2 {

  final case class Draw(redCubes: Long, blueCubes: Long, greenCubes: Long) {
    val power: Long = redCubes * blueCubes * greenCubes
  }
  object Draw {
    val redCubesRegex = "\\d+ red".r
    val blueCubesRegex = "\\d+ blue".r
    val greenCubesRegex = "\\d+ green".r
    def apply(s: String): Draw =
      val redCubes = redCubesRegex.findFirstIn(s).map(_.split(" ")(0).toInt).getOrElse(0)
      val blueCubes = blueCubesRegex.findFirstIn(s).map(_.split(" ")(0).toInt).getOrElse(0)
      val greenCubes = greenCubesRegex.findFirstIn(s).map(_.split(" ")(0).toInt).getOrElse(0)
      Draw(redCubes, blueCubes, greenCubes)
  }
  final case class Game(index: Int, draws: List[Draw]) {
    def isPossible(limit: Draw): Boolean =
      draws.forall { draw =>
        draw.redCubes <= limit.redCubes &&
        draw.blueCubes <= limit.blueCubes &&
        draw.greenCubes <= limit.greenCubes
      }

    def mininumDraw: Draw =
      draws.reduce { case (d1, d2) =>
        Draw(d1.redCubes.max(d2.redCubes), d1.blueCubes.max(d2.blueCubes), d1.greenCubes.max(d2.greenCubes))
      }
  }
  object Game {
    def apply(s: String): Game =
      val gameAndDraws = s.split(":")
      val index = gameAndDraws(0).split(" ")(1).toInt
      val draws = gameAndDraws(1).split(";")
      Game(index, draws.map(Draw(_)).toList)
  }
}
