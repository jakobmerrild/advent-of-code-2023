package days

class Day4(input: String) extends Puzzle {
  override def solve: String =
    input.split("\n").map(Day4.Card(_)).map(_.prize).sum.toString

  def processCard(
      card: Day4.Card,
      cardMap: Map[Int, Day4.Card],
      results: Map[Int, Int]
  ): Int =
    def aux(remainder: List[Day4.Card], acc: Int): Int =
      remainder match
        case head :: tail =>
          val memoizedResult = results.get(head.id)
          memoizedResult match
            case Some(result) => aux(tail, acc + result)
            case None => aux(tail ++ head.prizeCards.map(cardMap(_)), acc + 1)
        case Nil => acc
    aux(List(card), 0)

  override def solve2: String =
    val cards = input.split("\n").map(Day4.Card(_))
    val cardsMap = cards.map(c => c.id -> c).toMap
    cards
      .foldRight(Map.empty[Int, Int]) { case (card, acc) =>
        val result = processCard(card, cardsMap, acc)
        acc + (card.id -> result)
      }
      .values
      .sum
      .toString
}

object Day4 {
  final case class Card(
      id: Int,
      winningNumbers: Set[Int],
      myNumbers: Set[Int]
  ) {
    def prize: Int =
      val numberWinningNumbers = myNumbers.intersect(winningNumbers).size
      Math.pow(2, numberWinningNumbers - 1).toInt

    def prizeCards: List[Int] =
      val numberOfWinningNumbers = myNumbers.intersect(winningNumbers).size
      (id + 1 to id + numberOfWinningNumbers).toList
  }
  object Card {
    val cardRegex = """^Card\s+(\d+):([^|]+)\|(.*)$""".r
    def apply(s: String): Card = {
      s match {
        case cardRegex(id, winningNumbers, myNumbers) =>
          Card(
            id.toInt,
            winningNumbers.split(" ").filter(_.nonEmpty).map(_.toInt).toSet,
            myNumbers.split(" ").filter(_.nonEmpty).map(_.toInt).toSet
          )
        case _ => throw new IllegalArgumentException(s"Invalid card: $s")
      }
    }
  }
}
